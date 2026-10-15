# ABAP Logger

SAP Logging as painless as any other language.

ABAP Version: 702 or higher

See the [mission statement](docs/MISSION.md) 

## Features
  * Record message in [Application Log(BC-SRV-BAL)](https://help.sap.com/viewer/10a06f346c531014a346f3874a7621fd/7.0.38/en-US/4e21012c35d44180e10000000a15822b.html)
  * Display message

## Installation

- Install this project via [ABAPGit](http://abapgit.org).

**:warning: Migration Required :warning:**

On 2021, February 28 the folder logic was changed, and the abapGit may not able to perform this migration automatically. Therefore, you may need to follow the following steps:
1. Uninstall Repository (see [Uninstall repository](https://docs.abapgit.org/guide-online-uninstall.html)).
2. Reinstall ABAP-Logger:
   - online: see  [Install Online Repo](https://docs.abapgit.org/guide-online-install.html).
   - offline: see  [Install Offline Repo](https://docs.abapgit.org/guide-offline-install.html).

## Breaking Changes in v2

Version 2 removes deprecated APIs and separates logging from display. When upgrading from v1, update application code as follows (see [PR #189](https://github.com/ABAP-Logger/ABAP-Logger/pull/189)):

### Logger Creation

`zcl_logger=>new` and `zcl_logger=>open` are removed. Use `zcl_logger_factory=>create_log` and `zcl_logger_factory=>open_log`, which return `REF TO zif_logger`. Configure the former `auto_save` and `second_db_conn` options through `zcl_logger_factory=>create_settings( )->set_autosave( ... )` and `set_usage_of_secondary_db_conn( ... )`, and pass the settings to the factory.

### Error Handling

Failed Application Log operations now raise `zcx_logger` instead of being ignored, asserted, or displayed as a status message. `zcx_logger_display_profile` is removed; catch `zcx_logger` for display-profile errors as well. Handle these exceptions where the application should recover from logging or display failures.

### Deprecated Logging Shortcuts

`zif_logger_deprecated` and the methods `a`, `e`, `w`, `i`, and `s` are removed. Replace them with `abend`, `error`, `warning`, `info`, and `success`, respectively.

### Concrete-class aliases

Method and type aliases on `zcl_logger` are removed. Use a `REF TO zif_logger` for logging calls, or qualify interface methods on a concrete reference (for example, `log->zif_logger~add( ... )`). Reference message types through `zif_logger_log_object`.

### Display API

`fullscreen`, `popup`, `display_fullscreen`, `display_as_popup`, and `display_in_container` are removed from `zif_logger` and `zcl_logger`. Call `zcl_logger_ui=>display_fullscreen`, `zcl_logger_ui=>display_as_popup`, or `zcl_logger_ui=>display_in_container` with the logger in the `logger` parameter (and `container` for embedded display). `zcl_logger_factory=>create_ui( )` is removed, and `zcl_logger_ui` can no longer be instantiated directly. Each static display call creates and returns a `REF TO zif_logger_ui`; retain it to refresh an existing container through `ui->display_in_container( ... )`.

Create display profiles with `zcl_logger_ui=>create_display_profile( ... )` instead of `zcl_logger_factory=>create_display_profile( ... )`. The optional `profile` parameter of all display methods now expects `REF TO zif_logger_display_profile` instead of `bal_s_prof`: pass the profile object directly, without calling `profile->get( )`. Omitting it uses the default profile for the selected display mode. `zcl_logger_injector=>set_display_profile` is removed; pass a custom profile explicitly to the display call.

```abap
DATA profile TYPE REF TO zif_logger_display_profile.
DATA ui TYPE REF TO zif_logger_ui.

profile = zcl_logger_ui=>create_display_profile( i_no_tree = abap_true ).
ui = zcl_logger_ui=>display_in_container(
  container = container
  logger    = log
  profile   = profile ).

" Reuse the returned UI to refresh the same container after adding messages.
ui->display_in_container( container = container logger = log ).
```

### Other

- **Initial input:** `add` and the severity methods skip initial values, including empty strings, empty tables, initial structures, and numeric zero. Convert initial numeric values to nonempty text if they must be logged (for example, `log->info( '0' )`).
- **External log ID:** The `desc` parameter of `create_log` and `open_log` is renamed to `extnumber`, matching `balhdr-extnumber`. Update named arguments. The `description` parameter of `set_header` remains unchanged.
- **Log attributes:** `handle`, `db_number`, and `header` are removed from `zif_logger`. Use `get_handle( )`, `get_db_number( )`, and `get_header( )`. The attributes remain read-only on `zcl_logger`; `control_handle` is now private to the UI class.
- **Loggable objects:** `zif_loggable_object` is renamed to `zif_logger_log_object`. Update interface implementations, qualified method names, casts, and references to `ty_symsg`, `ty_message`, and `tty_messages`. Its `get_message_table` returning parameter is now `result`.
- **Trace logging:** `debug` is removed from `zif_logger`. Use `trace` instead.
- **Returning parameters:** Returning parameters across the logger, factory, settings, display-profile, and log-object APIs are renamed to `result`. Update explicit `RECEIVING` arguments and custom interface implementations or test doubles. Functional calls and method chaining use the same syntax. Settings and display-profile importing parameters retain their `i_` prefixes.

## Run Unit Tests

1. In transaction code `SLG0`, create for object `ABAPUNIT`. 
2. Launch unit test with `Ctrl` + `Alt` + `F10`.
