# ABAP Logger - Dokumentation

SAP Logging as painless as any other language.

**ABAP Version:** 702 oder höher

---

## 📋 Überblick

**ABAP Logger** ist ein modernes Logging-Framework für SAP ABAP, das die komplexe SAP Application Log (BAL) API vereinfacht. Es ermöglicht Entwicklern, Nachrichten in einer einzigen Codezeile zu protokollieren, anstatt umfangreich zu konfigurieren.

### Warum ABAP Logger?

Das native SAP Logging mit BAL-Funktionsmodulen erfordert viel Boilerplate-Code:

```abap
DATA: header TYPE bal_s_log,
      handle TYPE balloghndl,
      handles_to_save TYPE bal_t_logh.

header-object = 'ZINTERFACES'.
header-subobject = 'ACCOUNTING'.
header-extnumber = 'Stuff imported from legacy systems'.

CALL FUNCTION 'BAL_LOG_CREATE'
  EXPORTING
    i_s_log      = header
  IMPORTING
    e_log_handle = handle.

CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
  EXPORTING
    i_log_handle = handle
    i_msgty = 'E'
    i_text = 'You see, what had happened was...'.

CALL FUNCTION 'BAL_DB_SAVE'
  EXPORTING
    i_t_log_handle = handles_to_save.
```

Mit ABAP Logger wird dasselbe in einer einzigen Zeile erreicht:

```abap
log = zcl_logger_factory=>create_log( object = 'ZINTERFACES'
                                      subobject = 'ACCOUNTING'
                                      desc = 'Stuff imported from legacy systems' ).
log->e( 'You see, what had happened was...' ).
```

### Kernfeatures

- **✓ Einfache API** - Intuitives Interface mit kürzesten Methodennamen (e, w, i, s, a)
- **✓ Typ-Sicherheit** - Automatische Konvertierung verschiedener Datentypen
- **✓ SAP Integration** - Volle Integration mit SAP Application Log und Transaktionen SLG1/SLGD
- **✓ Method Chaining** - Verkettbare Methodenaufrufe für fluent API
- **✓ Konfigurierbar** - Verschiedene Einstellungen und Display-Profile
- **✓ Getestet** - Umfangreiche Unit Tests mit ABAP Unit

---

## 📦 Installation

### Via ABAPGit

1. Öffnen Sie **ABAPGit** (http://abapgit.org)
2. Klicken Sie auf **Clone Repository**
3. Geben Sie die Repository-URL ein
4. Wählen Sie ein Paket zum Installieren aus
5. Klicken Sie auf **Create Online Repository**

### Offline-Installation

1. Laden Sie das Repository herunter
2. Verwenden Sie die ABAPGit Offline-Installation
3. Weitere Informationen: [ABAPGit Offline-Guide](https://docs.abapgit.org/guide-offline-install.html)

### ⚠️ Migration Required

Wenn Sie eine ältere Version haben, müssen Sie möglicherweise:

1. Das Repository deinstallieren (siehe [Uninstall repository](https://docs.abapgit.org/guide-online-uninstall.html))
2. Neu installieren:
   - Online: siehe [Install Online Repo](https://docs.abapgit.org/guide-online-install.html)
   - Offline: siehe [Install Offline Repo](https://docs.abapgit.org/guide-offline-install.html)

---

## 🚀 Quick Start

### 1. Einfacher Log erstellen

```abap
DATA log TYPE REF TO zif_logger.

log = zcl_logger_factory=>create_log(
  object = 'ZINTERFACES'
  subobject = 'ACCOUNTING'
  desc = 'Stuff imported from legacy systems' ).

log->e( 'You see, what had happened was...' ).
```

### 2. Mit verschiedenen Log-Levels

```abap
log->s( 'Document 4800095710 created successfully' ).  "Success
log->i( 'Processing started...' ).                      "Information
log->w( 'Warning: deprecated field used' ).             "Warning
log->e( 'An error occurred!' ).                          "Error
log->a( 'Aborted due to missing data' ).                "Aborted
```

### 3. Method Chaining

```abap
zcl_logger_factory=>create_log( object = 'ZTEST' )
  ->e( 'Error 1' )
  ->w( 'Warning 2' )
  ->i( 'Info 3' )
  ->save().
```

### 4. Log anzeigen

```abap
log->display_fullscreen().      "Vollbildmodus
log->display_as_popup().         "Als Pop-up
log->display_in_container( c ).  "In Container
```

**Das war schon alles für ein einfaches Logging!** Die Logs werden automatisch in der SAP Application Log gespeichert und können in SLG1 angezeigt werden.

---

## 📚 API-Referenz

### Logging-Methoden

Die Hauptmethoden zum Hinzufügen von Log-Nachrichten. Alle Methoden unterstützen Method Chaining und geben `self` zurück.

| Methode | Typ | Beschreibung |
|---------|-----|-------------|
| `e( obj_to_log )` | **Error** | Fehler protokollieren (Fehlerfall) |
| `w( obj_to_log )` | **Warning** | Warnung protokollieren |
| `i( obj_to_log )` | **Info** | Informationsmeldung |
| `s( obj_to_log )` | **Success** | Erfolgsmeldung |
| `a( obj_to_log )` | **Aborted** | Abbruch-Meldung |
| `add( obj_to_log )` | **Generic** | Intelligente Nachricht (bestimmt Typ automatisch) |

### Status-Methoden

| Methode | Rückgabewert | Beschreibung |
|---------|--------------|-------------|
| `has_errors()` | `abap_bool` | Gibt TRUE zurück, wenn Log Fehler enthält |
| `has_warnings()` | `abap_bool` | Gibt TRUE zurück, wenn Log Warnungen enthält |
| `is_empty()` | `abap_bool` | Gibt TRUE zurück, wenn Log leer ist |
| `length()` | `i` | Anzahl der Einträge im Log |

### Speicher und Display-Methoden

| Methode | Parameter | Beschreibung |
|---------|-----------|-------------|
| `save()` | — | Speichert das Log in der Datenbank (nur wenn Auto-Save deaktiviert ist) |
| `display_fullscreen()` | `profile` (optional) | Zeigt Log im Vollbildmodus an |
| `display_as_popup()` | `profile` (optional) | Zeigt Log als Pop-up an |
| `display_in_container()` | `container, profile` | Zeigt Log in einem GUI Container an |
| `export_to_table()` | — | Exportiert Log als BAPIRET2-Tabelle |
| `free()` | — | Gibt die Log-Ressourcen frei |

### Factory-Methoden

| Methode | Beschreibung |
|---------|-------------|
| `zcl_logger_factory=>create_log()` | Erstellt ein neues Log |
| `zcl_logger_factory=>open_log()` | Öffnet ein bestehendes Log |
| `zcl_logger_factory=>open_log_by_db_number()` | Öffnet Log nach Datenbank-Nummer |
| `zcl_logger_factory=>create_settings()` | Erstellt ein Settings-Objekt |
| `zcl_logger_factory=>create_collection()` | Erstellt eine Log-Sammlung |
| `zcl_logger_factory=>create_display_profile()` | Erstellt ein Display-Profile |

### Datenattribute (Read-Only)

```abap
DATA log TYPE REF TO zif_logger.

log->handle          "BAL Log Handle
log->db_number       "Datenbank-Nummer
log->header          "BAL Log Header Struktur
log->control_handle  "BAL Kontrollierungshandle
```

---

## 💡 Praktische Beispiele

### Beispiel 1: BAPI-Rückgabe protokollieren

```abap
DATA: bapi_return TYPE bapiret2.

CALL FUNCTION 'SOME_BAPI'
  IMPORTING
    e_return = bapi_return.

log = zcl_logger_factory=>create_log( object = 'ZBAPI' ).
log->add( bapi_return ).

IF log->has_errors( ).
  log->display_as_popup( ).
ELSE.
  log->save( ).
ENDIF.
```

### Beispiel 2: Fehlerbehandlung mit Exceptions

```abap
TRY.
    PERFORM some_operation.
  CATCH cx_my_exception INTO ex.
    log->e( ex ).
    log->w( 'Operation fehlgeschlagen' ).
ENDTRY.

IF log->has_errors( ).
  log->display_fullscreen( ).
ENDIF.
```

### Beispiel 3: Batch-Verarbeitung mit Log-Sammlung

```abap
DATA: logs TYPE REF TO zif_logger_collection.

logs = zcl_logger_factory=>create_collection( ).

LOOP AT document_table INTO document.
  DATA log TYPE REF TO zif_logger.
  log = zcl_logger_factory=>create_log(
    object = 'ZDOC'
    desc = document-id ).

  IF process_document( document ) EQ abap_false.
    log->e( 'Dokument konnte nicht verarbeitet werden' ).
  ELSE.
    log->s( 'Dokument erfolgreich verarbeitet' ).
  ENDIF.

  logs->add( log ).
ENDLOOP.

logs->display( ).
```

### Beispiel 4: BDC-Fehler protokollieren

```abap
DATA: bdc_messages TYPE baapi_tab_messages.

CALL TRANSACTION 'CO07' USING bdc_tab MESSAGES INTO bdc_messages.

log = zcl_logger_factory=>create_log(
  object = 'ZBDC'
  subobject = 'CO07' ).

log->add( bdc_messages ).
log->display_as_popup( ).
```

### Beispiel 5: Kontext-Informationen hinzufügen

```abap
DATA: document TYPE zs_document.

log = zcl_logger_factory=>create_log( object = 'ZDOC' ).

log->w( obj_to_log = 'Dokument mit Warnungen erstellt'
        context = document ).

log->save( ).
```

### Beispiel 6: Log mit Einstellungen

```abap
DATA: settings TYPE REF TO zif_logger_settings.

settings = zcl_logger_factory=>create_settings( ).
"settings->set_auto_save( abap_true ).

log = zcl_logger_factory=>create_log(
  object = 'ZTEST'
  settings = settings ).

log->e( 'Fehler 1' ).
log->w( 'Warnung 1' ).
log->save( ).
```

---

## 🏗️ Architektur & Komponenten

### Komponenten-Übersicht

```
┌─────────────────────────────────────────┐
│        zcl_logger_factory               │
│  Statische Factory-Methoden             │
└──────────────┬──────────────────────────┘
               │
               ├──► zcl_logger (Implementierung)
               │    └─ zif_logger (Interface)
               │
               ├──► zcl_logger_settings
               │    └─ zif_logger_settings
               │
               ├──► zcl_logger_collection
               │    └─ zif_logger_collection
               │
               ├──► zcl_logger_display_profile
               │    └─ zif_logger_display_profile
               │
               └──► Exception Handler
                    └─ zcx_logger
```

### Schichten-Modell

- **Presentation Layer** - Display-Profile und Container-Integration für verschiedene UI-Präsentationen
- **Business Logic** - Kernlogik für Log-Verwaltung, Nachricht-Verarbeitung und Kontext-Handling
- **Data Access Layer** - Integration mit SAP BAL (Application Log) Framework
- **Type Handler** - Automatische Typkonvertierung für verschiedene Datentypen (BAPI, HR, Exceptions, etc.)

### Haupt-Klassen

#### zcl_logger
Die Hauptklasse, die das `zif_logger` Interface implementiert. Verwaltet alle Log-Operationen.

- Message-Handling (e, w, i, s, a)
- Status-Abfragen
- Display-Funktionalität
- Speicherverwaltung

#### zcl_logger_factory
Statische Factory-Klasse für die Erstellung von Logger-Instanzen.

- Neue Logs erstellen
- Bestehende Logs öffnen
- Settings und Profile konfigurieren

#### zcl_logger_settings
Konfigurationsobjekt für Logger-Verhalten.

- Auto-Save aktivieren/deaktivieren
- Andere Runtime-Konfigurationen

#### zcl_logger_collection
Verwaltet mehrere Logger-Instanzen.

- Mehrere Logs sammeln
- Batch-Anzeige

#### zcl_logger_display_profile
Konfiguriert die Anzeige-Optionen für Log-Display.

- Detail-Level
- UI-Optionen (Tree, Popup, etc.)

---

## 🧪 Unit Testing

### Test-Ausführung

1. Öffnen Sie Transaktion `SLG0`
2. Erstellen Sie ein Objekt für `ABAPUNIT`
3. Starten Sie die Unit Tests mit `Ctrl + Alt + F10`

### Test-Struktur

Das Repository enthält umfangreiche Unit Tests in den folgenden Klassen:

- `zcl_logger.clas.testclasses.abap` - Tests für Kernfunktionalität
- `zcl_logger_settings.clas.testclasses.abap` - Tests für Settings

### Testbeispiel

```abap
CLASS ltc_logger_test DEFINITION FOR TESTING.
  PRIVATE SECTION.
    METHODS test_simple_error_logging FOR TESTING.
ENDCLASS.

CLASS ltc_logger_test IMPLEMENTATION.
  METHOD test_simple_error_logging.
    DATA: log TYPE REF TO zif_logger.

    log = zcl_logger_factory=>create_log(
      object = 'ZTEST'
      subobject = 'UNIT_TEST' ).

    log->e( 'Test error' ).

    cl_abap_unit_assert=>assert_true(
      act = log->has_errors( ) ).
  ENDMETHOD.
ENDCLASS.
```

> **Hinweis:** Die Tests verwenden ABAP Unit Framework und erfordern die Installation des Projekts via ABAPGit.

---

## 📖 Zusätzliche Ressourcen

### Externe Links

- [GitHub Repository](https://github.com/ABAP-Logger)
- [Demo Repository mit Beispielen](https://github.com/ABAP-Logger/demos)
- [ABAPGit Dokumentation](https://docs.abapgit.org)
- [SAP Application Log (BAL) Dokumentation](https://help.sap.com/viewer/10a06f346c531014a346f3874a7621fd)

### SAP Transaktionen

- `SLG0` - Application Log Konfiguration
- `SLG1` - Application Logs anzeigen
- `SLGD` - Logs durchsuchen und filtern
- `SE24` - Class Builder (für Quellcode-Einsicht)

### Lizenz

Dieses Projekt ist unter der MIT License lizenziert. Details finden Sie in der LICENSE Datei des Repositories.

---

*Dokumentation aktualisiert: Juli 2026*

Für weitere Informationen besuchen Sie das [GitHub Repository](https://github.com/ABAP-Logger)
