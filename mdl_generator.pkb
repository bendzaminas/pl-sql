CREATE OR REPLACE PACKAGE mdl_package_generator AUTHID CURRENT_USER IS

  c_helper_abr     CONSTANT VARCHAR2(10) := 'HLP';
  c_model_abr      CONSTANT VARCHAR2(10) := 'MDL';
  c_controller_abr CONSTANT VARCHAR2(10) := 'CTRL';
  c_config_abr     CONSTANT VARCHAR2(10) := 'CFG';
  c_router_abr     CONSTANT VARCHAR2(10) := 'ROUTE';
  c_language_abr   CONSTANT VARCHAR2(10) := 'LANG';
  c_ds_abr         CONSTANT VARCHAR2(10) := 'DS';

  c_view_abr    CONSTANT VARCHAR2(10) := 'V';
  c_trigger_abr CONSTANT VARCHAR2(10) := 'IU';

  PROCEDURE mdl_head(p_owner    IN VARCHAR2,
                     p_table    IN VARCHAR2,
                     p_recreate IN VARCHAR2 DEFAULT 'N');

  PROCEDURE mdl_body(p_owner         IN VARCHAR2,
                     p_table         IN VARCHAR2,
                     p_recreate      IN VARCHAR2 DEFAULT 'N',
                     p_upper_strings IN BOOLEAN DEFAULT TRUE);

  PROCEDURE create_trigger(p_owner         IN VARCHAR2,
                           p_table         IN VARCHAR2,
                           p_recreate      IN VARCHAR2 DEFAULT 'N',
                           p_upper_strings IN VARCHAR2 DEFAULT 'Y');

END;

/

CREATE OR REPLACE PACKAGE BODY mdl_package_generator IS

  PROCEDURE execute_sql(p_statement IN VARCHAR2) AS
  
  BEGIN
  
    dbms_output.put_line(p_statement);
    EXECUTE IMMEDIATE p_statement;
  END;

  FUNCTION check_primary_key_sequence(p_owner IN VARCHAR2,
                                      p_seq   IN VARCHAR2) RETURN BOOLEAN AS
    o_dummy NUMBER;
  BEGIN
  
    SELECT 1
      INTO o_dummy
      FROM all_sequences a
     WHERE a.sequence_owner = upper(p_owner)
       AND a.sequence_name = upper(p_seq);
  
    RETURN TRUE;
  
  END;

  FUNCTION transform_name(p_name IN VARCHAR2,
                          p_size IN NUMBER) RETURN VARCHAR2 AS
    new_name     VARCHAR2(100);
    replace_name VARCHAR2(100);
  BEGIN
  
    new_name := p_name;
  
    IF length(p_name) <= p_size THEN
      new_name := p_name;
    ELSE
    
      FOR element IN (SELECT regexp_substr(p_name, '[^_]+', 1, LEVEL) mean
                        FROM dual
                      CONNECT BY regexp_substr(p_name, '[^_]+', 1, LEVEL) IS NOT NULL) LOOP
      
        replace_name := element.mean || '_';
      
        IF length(new_name) > p_size THEN
          new_name := REPLACE(new_name, replace_name);
        END IF;
      
        EXIT WHEN length(new_name) <= p_size;
      
      END LOOP;
    
    END IF;
  
    RETURN new_name;
  
  END;

  FUNCTION decode_type(p_type IN VARCHAR2) RETURN VARCHAR2 AS
  
  BEGIN
  
    IF TRIM(p_type) IS NULL THEN
      raise_application_error(-20343, 'Nepateikti visi būtini parametrai');
    END IF;
  
    RETURN CASE WHEN lower(TRIM(p_type)) IN('helper', 'help', 'hlp', 'h') THEN c_helper_abr WHEN lower(TRIM(p_type)) IN('model',
                                                                                                                        'mod',
                                                                                                                        'mdl',
                                                                                                                        'm') THEN c_model_abr WHEN lower(TRIM(p_type)) IN('controller',
                                                                                                                                                                          'control',
                                                                                                                                                                          'con',
                                                                                                                                                                          'cntrl',
                                                                                                                                                                          'cont') THEN c_controller_abr WHEN lower(TRIM(p_type)) IN('config',
                                                                                                                                                                                                                                    'cfg',
                                                                                                                                                                                                                                    'conf') THEN c_config_abr WHEN lower(TRIM(p_type)) IN('router',
                                                                                                                                                                                                                                                                                          'route',
                                                                                                                                                                                                                                                                                          'rt',
                                                                                                                                                                                                                                                                                          'r') THEN c_router_abr WHEN lower(TRIM(p_type)) IN('language',
                                                                                                                                                                                                                                                                                                                                             'lang',
                                                                                                                                                                                                                                                                                                                                             'lng',
                                                                                                                                                                                                                                                                                                                                             'l') THEN c_language_abr WHEN lower(TRIM(p_type)) IN('view',
                                                                                                                                                                                                                                                                                                                                                                                                  'v') THEN c_view_abr WHEN lower(TRIM(p_type)) IN('trigger',
                                                                                                                                                                                                                                                                                                                                                                                                                                                   'trg',
                                                                                                                                                                                                                                                                                                                                                                                                                                                   't') THEN c_trigger_abr WHEN lower(TRIM(p_type)) IN('data_sync',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       'data',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       'sync',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       'ds') THEN c_ds_abr END;
  
  END;

  FUNCTION get_primary_key_column(p_owner IN VARCHAR2,
                                  p_table IN VARCHAR2) RETURN VARCHAR2 AS
    l_pk VARCHAR(30);
  BEGIN
  
    BEGIN
      SELECT b.column_name
        INTO l_pk
        FROM all_constraints a
        JOIN all_cons_columns b
          ON a.constraint_name = b.constraint_name
         AND a.owner = b.owner
       WHERE a.owner = upper(p_owner)
         AND a.table_name = upper(p_table)
         AND a.constraint_type = 'P';
    
      RETURN l_pk;
    
    END;
  END;

  FUNCTION get_primary_key_sequence(p_owner IN VARCHAR2,
                                    p_table IN VARCHAR2) RETURN VARCHAR2 AS
    l_pk  VARCHAR(30);
    l_seq VARCHAR2(30);
  BEGIN
  
    l_pk := get_primary_key_column(p_owner, p_table);
  
    l_seq := upper(l_pk || '#');
  
    RETURN l_seq;
  
  END;

  FUNCTION get_object_name(p_table IN VARCHAR2,
                           p_type  IN VARCHAR2) RETURN VARCHAR2 AS
  BEGIN
  
    IF TRIM(p_table) IS NULL OR
       TRIM(p_type) IS NULL THEN
      raise_application_error(-20343, 'Nepateikti visi būtini parametrai');
    END IF;
  
    RETURN CASE decode_type(p_type) WHEN c_helper_abr THEN c_helper_abr || '_' || upper(p_table) WHEN c_model_abr THEN c_model_abr || '_' || upper(p_table) WHEN c_controller_abr THEN c_controller_abr || '_' || upper(p_table) WHEN c_config_abr THEN c_config_abr || '_' || upper(p_table) WHEN c_router_abr THEN c_router_abr || '_' || upper(p_table) WHEN c_language_abr THEN c_language_abr || '_' || upper(p_table) WHEN c_view_abr THEN c_view_abr || '_' || upper(p_table) WHEN c_ds_abr THEN c_ds_abr || '_' || upper(p_table) WHEN c_trigger_abr THEN upper(p_table) || '_' || c_trigger_abr END;
  
  END;

  PROCEDURE mdl_head(p_owner    IN VARCHAR2,
                     p_table    IN VARCHAR2,
                     p_recreate IN VARCHAR2 DEFAULT 'N') AS
  
    l_sql VARCHAR2(32765);
  
    l_pkg VARCHAR2(30) := get_object_name(p_table, 'model');
  
    CURSOR c_columns_with_precision(p_owner IN VARCHAR2,
                                    p_table IN VARCHAR2) IS
      SELECT a.column_name,
             CASE
               WHEN a.data_type = 'NUMBER' AND
                    a.data_precision IS NOT NULL THEN
                a.data_type || '(' || a.data_precision || ')'
               WHEN a.data_type = 'VARCHAR2' THEN
                a.data_type || '(' || a.char_length || ')'
               WHEN a.data_type = 'RAW' THEN
                a.data_type || '(' || a.data_length || ')'
               ELSE
                a.data_type
             END data_type,
             column_id nr,
             rownum eil,
             COUNT(*) over() nr_max
        FROM all_tab_cols a
       WHERE a.owner = upper(p_owner)
         AND a.table_name = upper(p_table)
         AND a.hidden_column = 'NO'
         AND a.virtual_column = 'NO'
       ORDER BY a.column_id;
  
    CURSOR c_unique_key(p_owner IN VARCHAR2,
                        p_table IN VARCHAR2) IS
      SELECT owner,
             constraint_name,
             rownum eil,
             nr
        FROM (SELECT a.owner,
                     a.constraint_name,
                     COUNT(*) nr
                FROM all_constraints a
                JOIN all_cons_columns b
                  ON a.constraint_name = b.constraint_name
                 AND a.owner = b.owner
               WHERE a.owner = upper(p_owner)
                 AND a.table_name = upper(p_table)
                 AND a.constraint_type = 'U'
                 AND b.column_name NOT LIKE 'VALID%'
               GROUP BY a.owner,
                        a.constraint_name);
  
    CURSOR c_constraint_cols(p_owner      IN VARCHAR2,
                             p_constraint IN VARCHAR) IS
      SELECT b.column_name,
             rownum eil,
             COUNT(*) over() nr
        FROM all_cons_columns b
       WHERE b.constraint_name = p_constraint
         AND b.owner = p_owner
         AND b.column_name NOT LIKE 'VALID%';
  
  BEGIN
  
    IF TRIM(p_owner) IS NULL THEN
      raise_application_error(-20343, 'Perduokite schemos pavadinimą.');
    END IF;
  
    IF TRIM(p_table) IS NULL THEN
      raise_application_error(-20343, 'Perduokite lentelės pavadinimą.');
    END IF;
  
    IF p_recreate = 'T' THEN
      l_sql := 'CREATE OR REPLACE ';
    ELSE
      l_sql := 'CREATE ';
    END IF;
  
    l_sql := l_sql || 'PACKAGE ' || lower(p_owner) || '.' || lower(l_pkg) || ' AUTHID DEFINER IS
      -- *****************************************************************
      -- Description: lentelės ' || upper(p_table) || ' MDL paketas
      --
      -- Input Parameters:
      --
      -- Output Parameters:
      --
      -- Error Conditions Raised:
      --
      -- Author:      Auto generator
      --
      -- Revision History
      -- Date            Author       Reason for Change
      -- ----------------------------------------------------------------
      -- ' || to_char(SYSDATE, 'YYYY-MM-DD') || '      Auto generator     sukurtas paketas
      -- *****************************************************************

      modified_by_other EXCEPTION;
  PRAGMA EXCEPTION_INIT(modified_by_other, -20001);

      /**
      * Funkcija yra naudojama gauti įrašo duomenys pagal PK
      *
      * PARAMETERS
      *   p_' || lower(get_primary_key_column(p_owner, p_table)) || ' - lentelės PK
      * RETURN
      *   ' || lower(p_table) || '%ROWTYPE
      * EXCEPTIONS
      *   no_data_found - jei pagal PK įrašas buvo nerastas
      * NOTES
      *
      *   DELCARE
      *      l_val ' || lower(p_table) || '%ROWTYPE;
      *   BEGIN
      *
      *      l_val := ' || lower(l_pkg) || '.get(p_' || lower(get_primary_key_column(p_owner, p_table)) ||
             ' => 123);
      *
      *      ...
      *   END;
      */

      FUNCTION get(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE) RETURN ' || lower(p_table) || '%ROWTYPE;
      ';
  
    l_sql := l_sql || '

      /**
      * Objekto versijos vidinio sistemos unikalaus identifikatoriaus sekančios reikšmės gavimas
      *
      * PARAMETERS
      *   None
      * RETURN
      *   ' || lower(p_table) || '.' || lower(get_primary_key_column(p_owner, p_table)) || '%TYPE
      * EXCEPTIONS
      *   Native
      * NOTES
      *
      *   DELCARE
      *      l_id ' || lower(p_table) || '.' || lower(get_primary_key_column(p_owner, p_table)) ||
             '%TYPE;
      *   BEGIN
      *
      *      l_id := ' || lower(l_pkg) || '.get_' || lower(get_primary_key_column(p_owner, p_table)) || ';
      *
      *      ...
      *   END;
      */

     FUNCTION get_' || transform_name(p_name => lower(get_primary_key_column(p_owner, p_table)), p_size => '26') ||
             ' RETURN ' || lower(p_table) || '.' || lower(get_primary_key_column(p_owner, p_table)) ||
             '%TYPE;
     PRAGMA RESTRICT_REFERENCES(get_' ||
             transform_name(p_name => lower(get_primary_key_column(p_owner, p_table)), p_size => '26') || ', WNDS, WNPS);';
  
    l_sql := l_sql || '

      /**
      * Procedūra skirta įrašo pašalinimui pagal PK
      *
      * PARAMETERS
      *   p_' || lower(get_primary_key_column(p_owner, p_table)) || ' - lentelės PK
      *   p_auto_transaction - požymis, kad tai autonominė tranzakcija (TRUE - autonominė, FALSE - neautonominė)
      * RETURN
      *   None
      * EXCEPTIONS
      *   no_data_found - jei pagal PK įrašas buvo nerastas
      * NOTES
      *
      *   DELCARE
      *
      *      ...
      *   BEGIN
      *
      *      ' || lower(l_pkg) || '.del(p_' || lower(get_primary_key_column(p_owner, p_table)) ||
             ' => 123, p_auto_transaction => TRUE);
      *
      *      ...
      *   END;
      */

      PROCEDURE del(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE, p_auto_transaction IN BOOLEAN DEFAULT FALSE);


        /**
        * Funkcija skirta naujų duomenų įrašymui
        *
        * PARAMETERS
        *   p_rec -  ' || lower(p_table) || '%ROWTYPE
        *   p_auto_transaction - požymis, kad tai autonominė tranzakcija (TRUE - autonominė, FALSE - neautonominė)
        * RETURN
        *   ' || lower(p_table) || '%ROWTYPE
        * EXCEPTIONS
        *   no_data_found - jei pagal PK įrašas buvo nerastas
        * NOTES
        *
        *   DELCARE
        *
        *      l_rec ' || lower(p_owner) || '.' || lower(p_table) || '%ROWTYPE;
        *   BEGIN
        *
        *      l_rec.valid_date := SYSDATE;
        *
        *      l_rec := ' || lower(l_pkg) || '.ins(p_rec => l_rec, p_auto_transaction => TRUE);
        *
        *      ...
        *   END;
        */

     FUNCTION ins(p_rec IN ' || lower(p_table) ||
             '%ROWTYPE,
                  p_auto_transaction IN BOOLEAN DEFAULT FALSE) RETURN ' || lower(p_table) || '%ROWTYPE;

      /**
      * Funkcija skirta duomenų atnaujinimui pagal PK
      *
      * PARAMETERS
      *   p_' || lower(get_primary_key_column(p_owner, p_table)) || ' - lentelės PK
      *   p_rec - ' || lower(p_table) || '%ROWTYPE
      *   p_auto_transaction - požymis, kad tai autonominė tranzakcija (TRUE - autonominė, FALSE - neautonominė)
      * RETURN
      *   ' || lower(p_table) || '%ROWTYPE
      * EXCEPTIONS
      *   no_data_found - jei pagal PK įrašas buvo nerastas
      * NOTES
      *
      *   DELCARE
      *
      *      l_rec ' || lower(p_table) || '%ROWTYPE;
      *   BEGIN
      *
      *      l_rec.valid_date := SYSDATE;
      *
      *      l_rec := ' || lower(p_owner) || '.' || lower(l_pkg) || '.upd(p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ' => 123, p_rec => l_rec, p_auto_transaction => TRUE);
      *
      *      ...
      *   END;
      */

     FUNCTION upd(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE,
                  p_rec ' || lower(p_table) || '%ROWTYPE,
                  p_auto_transaction IN BOOLEAN DEFAULT FALSE)
       RETURN ' || lower(p_table) || '%ROWTYPE;

END;';
    execute_sql(l_sql);
  
  END;

  PROCEDURE mdl_body(p_owner         IN VARCHAR2,
                     p_table         IN VARCHAR2,
                     p_recreate      IN VARCHAR2 DEFAULT 'N',
                     p_upper_strings IN BOOLEAN DEFAULT TRUE) AS
  
    l_sql VARCHAR2(32765);
  
    l_pkg VARCHAR2(30) := get_object_name(p_table, 'model');
  
    l_pk VARCHAR2(30);
  
    CURSOR c_columns(p_owner IN VARCHAR2,
                     p_table IN VARCHAR2) IS
      SELECT a.column_name,
             a.data_type,
             column_id nr,
             rownum eil,
             COUNT(*) over() nr_max
        FROM all_tab_cols a
       WHERE a.owner = upper(p_owner)
         AND a.table_name = upper(p_table)
         AND a.hidden_column = 'NO'
         AND a.virtual_column = 'NO'
       ORDER BY column_id;
  
    CURSOR c_unique_key(p_owner IN VARCHAR2,
                        p_table IN VARCHAR2) IS
      SELECT owner,
             constraint_name,
             rownum eil,
             nr
        FROM (SELECT a.owner,
                     a.constraint_name,
                     COUNT(*) nr
                FROM all_constraints a
                JOIN all_cons_columns b
                  ON a.constraint_name = b.constraint_name
                 AND a.owner = b.owner
               WHERE a.owner = upper(p_owner)
                 AND a.table_name = upper(p_table)
                 AND a.constraint_type = 'U'
                 AND b.column_name NOT LIKE 'VALID%'
               GROUP BY a.owner,
                        a.constraint_name);
  
    CURSOR c_foreign_key(p_owner IN VARCHAR2,
                         p_table IN VARCHAR2) IS
      SELECT c.table_name,
             c.column_name,
             a.constraint_name,
             rownum nr,
             COUNT(*) over() max_nr
        FROM all_constraints a
        JOIN all_cons_columns b
          ON a.r_constraint_name = b.constraint_name
         AND a.owner = b.owner
        JOIN all_cons_columns c
          ON a.constraint_name = c.constraint_name
         AND a.owner = c.owner
       WHERE a.constraint_type = 'R'
         AND a.owner = upper(p_owner)
         AND b.table_name = upper(p_table);
  
    CURSOR c_constraint_cols(p_owner      IN VARCHAR2,
                             p_constraint IN VARCHAR) IS
      SELECT b.column_name,
             rownum eil,
             COUNT(*) over() nr
        FROM all_cons_columns b
       WHERE b.constraint_name = p_constraint
         AND b.owner = p_owner
         AND b.column_name NOT LIKE 'VALID%';
  
  BEGIN
  
    IF TRIM(p_owner) IS NULL THEN
      raise_application_error(-20343, 'Perduokite schemos pavadinimą.');
    END IF;
  
    IF TRIM(p_table) IS NULL THEN
      raise_application_error(-20343, 'Perduokite lentelės pavadinimą.');
    END IF;
  
    IF p_recreate = 'T' THEN
      l_sql := 'CREATE OR REPLACE ';
    ELSE
      l_sql := 'CREATE ';
    END IF;
  
    l_pk := get_primary_key_column(p_owner, p_table);
  
    l_sql := l_sql || 'PACKAGE BODY ' || lower(p_owner) || '.' || lower(l_pkg) || ' IS ';
  
    --GET function generation
    l_sql := l_sql || '

     FUNCTION get(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE) RETURN ' || lower(p_table) || '%ROWTYPE IS

          CURSOR c(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE) IS
               SELECT /*+ result_cache first_rows(1) */ * ';
  
    l_sql := l_sql || '
                 FROM ' || lower(p_table) || '
                WHERE ' || lower(l_pk) || ' = p_' || lower(get_primary_key_column(p_owner, p_table)) || '
         AND rownum = 1;

         l_rec ' || lower(p_table) || '%ROWTYPE;

     BEGIN
          OPEN c(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ');
          FETCH c
               INTO l_rec;
          CLOSE c;

          IF l_rec.' || lower(l_pk) || ' IS NULL THEN
               RAISE no_data_found;
          END IF;

          RETURN l_rec;

     END;

     ';
  
    --GET_&abr_ID function generation
    l_sql := l_sql || '

     FUNCTION get_' || transform_name(p_name => lower(l_pk), p_size => 26) || ' RETURN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE IS

          CURSOR c IS
               SELECT ' || lower(get_primary_key_sequence(p_owner, p_table)) || '.nextval
                 FROM dual;

          l_result ' || lower(p_table) || '.' || lower(get_primary_key_column(p_owner, p_table)) || '%TYPE;

     BEGIN
          OPEN c;
          FETCH c
               INTO l_result;
          CLOSE c;

          RETURN l_result;
     END;

     ';
  
    --DEL function generation
    l_sql := l_sql || '

     PROCEDURE no_autonomous_del(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE) IS

     BEGIN
               DELETE /*+first_rows(1)*/
               ' || p_table || ' t
                WHERE ' || 't.' || lower(get_primary_key_column(p_owner, p_table)) || ' = p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ' AND rownum = 1;

               IF SQL%NOTFOUND THEN
                     RAISE no_data_found;
               END IF;


     END;' || chr(10) || chr(10);
  
    l_sql := l_sql || '

      PROCEDURE autonomous_del(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE) IS

    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN

    no_autonomous_del(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ');

    COMMIT;


  END;' || chr(10) || chr(10);
  
    --DEL function generation
    l_sql := l_sql || '
     PROCEDURE del(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE,
                   p_auto_transaction IN BOOLEAN DEFAULT FALSE) IS

     BEGIN

         IF p_auto_transaction THEN

            autonomous_del(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ');

          ELSE

            no_autonomous_del(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ');

          END IF;
     END;' || chr(10) || chr(10);
  
    l_sql := l_sql || '
     FUNCTION no_autonomous_ins(p_rec IN ' || lower(p_table) || '%ROWTYPE)
     RETURN ' || lower(p_table) || '%ROWTYPE
      IS

        l_rec ' || lower(p_table) || '%ROWTYPE;
     BEGIN

          INSERT INTO ' || lower(p_table) || ' t
    VALUES p_rec
    RETURNING t.' || lower(get_primary_key_column(p_owner, p_table)) || ' INTO l_rec.' ||
             lower(get_primary_key_column(p_owner, p_table)) || ';

    RETURN get(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => l_rec.' ||
             lower(get_primary_key_column(p_owner, p_table)) || ');

     END;' || chr(10) || chr(10);
  
    l_sql := l_sql || '
     FUNCTION autonomous_ins(p_rec IN ' || lower(p_table) || '%ROWTYPE)
                   RETURN ' || lower(p_table) || '%ROWTYPE IS
                   PRAGMA AUTONOMOUS_TRANSACTION;

        l_rec ' || lower(p_table) || '%ROWTYPE;
     BEGIN

        l_rec := no_autonomous_ins(p_rec => p_rec);

    COMMIT;

    RETURN l_rec;

     END;' || chr(10) || chr(10);
  
    l_sql := l_sql || '
     FUNCTION ins(p_rec IN ' || lower(p_table) ||
             '%ROWTYPE,
                   p_auto_transaction IN BOOLEAN DEFAULT FALSE) RETURN ' || lower(p_table) || '%ROWTYPE IS

        l_rec ' || lower(p_table) || '%ROWTYPE;
     BEGIN

        IF p_auto_transaction THEN

          l_rec := autonomous_ins(p_rec => p_rec);

        ELSE

          l_rec := no_autonomous_ins(p_rec => p_rec);

        END IF;

        RETURN l_rec;

     END;' || chr(10) || chr(10);
  
    l_sql := l_sql || '

     FUNCTION no_autonomous_upd(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE,
                  p_rec IN ' || lower(p_table) || '%ROWTYPE)
       RETURN ' || lower(p_table) || '%ROWTYPE IS

          l_old ' || lower(p_table) || '%ROWTYPE;
          l_rec ' || lower(p_table) || '%ROWTYPE;
     BEGIN

          l_old := get(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ');

         UPDATE ' || lower(p_table) || ' t
       SET row = p_rec
     WHERE t.' || lower(get_primary_key_column(p_owner, p_table)) || ' = p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || '
    RETURNING t.number_of_times_modified, t.' || lower(get_primary_key_column(p_owner, p_table)) ||
             ' INTO l_rec.number_of_times_modified, l_rec.' || lower(get_primary_key_column(p_owner, p_table)) || ';


               IF SQL%NOTFOUND THEN
                    RAISE no_data_found;
               END IF;

                 IF l_old.number_of_times_modified + 1 != l_rec.number_of_times_modified THEN
                         ROLLBACK;
                         RAISE modified_by_other;
                    END IF;

      RETURN get(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => l_rec.' ||
             lower(get_primary_key_column(p_owner, p_table)) || ');

     END;' || chr(10) || chr(10);
  
    l_sql := l_sql || '

       FUNCTION autonomous_upd(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE,
                          p_rec                 IN ' || lower(p_table) || '%ROWTYPE)
    RETURN ' || lower(p_table) || '%ROWTYPE IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    l_rec ' || lower(p_table) || '%ROWTYPE;
  BEGIN
    l_rec := no_autonomous_upd(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ',
                 p_rec                 => p_rec);

    COMMIT;

    RETURN l_rec;

  END;' || chr(10) || chr(10);
  
    l_sql := l_sql || '
     FUNCTION upd(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' IN ' || lower(p_table) || '.' ||
             lower(get_primary_key_column(p_owner, p_table)) || '%TYPE,
                  p_rec IN ' || lower(p_table) || '%ROWTYPE,
                  p_auto_transaction IN BOOLEAN DEFAULT FALSE)
       RETURN ' || lower(p_table) || '%ROWTYPE IS

     BEGIN

         IF p_auto_transaction THEN

            RETURN autonomous_upd(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ', p_rec => p_rec);

          ELSE

            RETURN no_autonomous_upd(p_' || lower(get_primary_key_column(p_owner, p_table)) || ' => p_' ||
             lower(get_primary_key_column(p_owner, p_table)) || ', p_rec => p_rec);

          END IF;

     END;' || chr(10) || chr(10);
  
    l_sql := l_sql || 'END;';
  
    execute_sql(l_sql);
  
  END;

  PROCEDURE create_trigger(p_owner         IN VARCHAR2,
                           p_table         IN VARCHAR2,
                           p_recreate      IN VARCHAR2 DEFAULT 'N',
                           p_upper_strings IN VARCHAR2 DEFAULT 'Y') AS
  
    l_sql VARCHAR2(32765);
  
    CURSOR c_varchar_cols(p_owner IN VARCHAR2,
                          p_table IN VARCHAR2) IS
      SELECT a.column_name,
             column_id nr
        FROM all_tab_cols a
       WHERE a.owner = upper(p_owner)
         AND a.table_name = upper(p_table)
         AND a.hidden_column = 'NO'
         AND a.virtual_column = 'NO'
         AND a.data_type = 'VARCHAR2'
         AND a.column_name NOT IN ('USER_CREATED', 'USER_MODIFIED')
       ORDER BY a.column_id;
  
    CURSOR c_primary_key(p_owner IN VARCHAR2,
                         p_table IN VARCHAR2) IS
      SELECT b.column_name
        FROM all_constraints a
        JOIN all_cons_columns b
          ON a.constraint_name = b.constraint_name
         AND a.owner = b.owner
       WHERE a.owner = upper(p_owner)
         AND a.table_name = upper(p_table)
         AND a.constraint_type = 'P';
  
  BEGIN
  
    IF TRIM(p_owner) IS NULL THEN
      raise_application_error(-20343, 'Perduokite schemos pavadinimą.');
    END IF;
  
    IF TRIM(p_table) IS NULL THEN
      raise_application_error(-20343, 'Perduokite lentelės pavadinimą.');
    END IF;
  
    IF p_recreate = 'T' THEN
      l_sql := 'CREATE OR REPLACE ';
    ELSE
      l_sql := 'CREATE ';
    END IF;
  
    l_sql := l_sql || 'TRIGGER ' || lower(p_owner) || '.' || lower(p_table) || '_iu' || chr(10) || chr(9) ||
             'BEFORE INSERT OR UPDATE ON ' || lower(p_owner) || '.' || lower(p_table) || chr(10) || chr(9) ||
             'REFERENCING NEW AS NEW OLD AS OLD' || chr(10) || chr(9) || 'FOR EACH ROW' || chr(10) || chr(10) ||
            
             'BEGIN' || chr(10) || chr(9) || 'IF inserting THEN' || chr(10) || chr(10) || chr(9) || chr(9) ||
            
             ':new.date_created := SYSDATE;' || chr(10) || chr(9) || chr(9) || ':new.user_created := USER;' || chr(10) || chr(9) ||
             chr(9) || ':new.number_of_times_modified := 0;' || chr(10) || chr(9) || chr(9);
  
    IF upper(p_upper_strings) = 'Y' THEN
    
      FOR i IN c_varchar_cols(p_owner, p_table) LOOP
        l_sql := l_sql || ':new.' || lower(i.column_name) || ' := upper(:new.' || lower(i.column_name) || ');' || chr(10) ||
                 chr(9) || chr(9);
      END LOOP;
    
    END IF;
  
    l_sql := l_sql || chr(10);
  
    FOR i IN c_primary_key(p_owner, p_table) LOOP
      l_sql := l_sql || 'IF :new.' || lower(i.column_name) || ' IS NULL THEN' || chr(10) || chr(9) || chr(9) || chr(9) || ':new.' ||
               lower(i.column_name) || ' := ' || lower(get_primary_key_sequence(p_owner, p_table)) || '.nextval;' || chr(10) ||
               chr(9) || chr(9) || 'END IF;' || chr(10) || chr(10) || chr(9);
    END LOOP;
  
    l_sql := l_sql || 'ELSIF updating THEN' || chr(10) || chr(10) || chr(9) || chr(9) ||
            
             ':new.date_created := :old.date_created;' || chr(10) || chr(9) || chr(9) ||
             ':new.user_created := :old.user_created;' || chr(10) || chr(9) || chr(9) || ':new.date_modified := SYSDATE;' ||
             chr(10) || chr(9) || chr(9) || ':new.user_modified := USER;' || chr(10) || chr(9) || chr(9) ||
             ':new.number_of_times_modified := nvl(:old.number_of_times_modified,0) + 1;' || chr(10) || chr(9) || chr(9);
  
    IF upper(p_upper_strings) = 'Y' THEN
    
      FOR i IN c_varchar_cols(p_owner, p_table) LOOP
        l_sql := l_sql || ':new.' || lower(i.column_name) || ' := upper(:new.' || lower(i.column_name) || ');' || chr(10) ||
                 chr(9) || chr(9);
      END LOOP;
    
    END IF;
  
    l_sql := l_sql || chr(10);
  
    l_sql := l_sql || 'END IF;' || chr(10) || chr(10) ||
            
             'END;';
  
    execute_sql(l_sql);
  
  END;

  PROCEDURE create_sequence(p_owner IN VARCHAR2,
                            p_table IN VARCHAR2) AS
  
    l_sql VARCHAR2(32765);
  
    k1 NUMBER;
  
  BEGIN
  
    IF TRIM(p_owner) IS NULL THEN
      raise_application_error(-20343, 'Perduokite schemos pavadinimą.');
    END IF;
  
    IF TRIM(p_table) IS NULL THEN
      raise_application_error(-20343, 'Perduokite lentelės pavadinimą.');
    END IF;
  
    IF check_primary_key_sequence(p_owner, get_primary_key_sequence(p_owner, p_table)) THEN
      RETURN;
    ELSE
      l_sql := 'create sequence ' || upper(p_owner) || '.' || upper(p_table) || '
        minvalue 1
        maxvalue 999999999999
        start with 1
        increment by 1
        nocache';
    
      execute_sql(l_sql);
    END IF;
  
  END;

END;
