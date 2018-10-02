CREATE OR REPLACE PACKAGE mdl_package_generator AUTHID CURRENT_USER IS

  c_helper_abr     CONSTANT VARCHAR2(10) := 'HLP';
  c_model_abr      CONSTANT VARCHAR2(10) := 'MDL';
  c_controller_abr CONSTANT VARCHAR2(10) := 'CTRL';
  c_config_abr     CONSTANT VARCHAR2(10) := 'CFG';
  c_router_abr     CONSTANT VARCHAR2(10) := 'ROUTE';
  c_language_abr   CONSTANT VARCHAR2(10) := 'LANG';
  c_ds_abr         CONSTANT VARCHAR2(10) := 'DS';

  c_view_abr    CONSTANT VARCHAR2(10) := 'V';
  c_trigger_abr 

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
