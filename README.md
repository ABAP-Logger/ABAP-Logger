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
2. Reinstall ABAP-Logger (see:
   - online: see  [Install Online Repo](https://docs.abapgit.org/guide-online-install.html).
   - offline: see  [Install Offline Repo](https://docs.abapgit.org/guide-offline-install.html).

## Run Unit Tests
1. In transaction code SLG0, create the Subobject `LOGGER` for Object `ABAPUNIT`. 
2. Launch unit test with `Ctrl` + `Alt` + `F10` 
