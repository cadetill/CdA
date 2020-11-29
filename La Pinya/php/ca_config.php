<?php

/**
 * Description of config
 *
 * @author cadetill
 */
class ca_config {
  // database connection variables
  public $dbserver = 'mariadb';
  public $dbnamebd = 'cda';
  public $dbuser = 'cdasql';
  public $dbpas = '0_ujRTnp';
  public $dbprefix = 'ca_';
  
  public $debugMode = false;
  
  // returned Json errors
  public $errBadRequest = '{"error": "bad request"}';
  public $errNoResult = '{"error": "no result"}';
  public $errFuncNotFound = '{"error":"function not found","errFunc":"%s"}';
  public $errBadParameter = '{"error":"bad parameter","errField":"%s"}';
  public $errRecordExists = '{"error":"record already exists","errTable":"%s","errRecordCount":"%s"}';
  public $errRecordNotExists = '{"error":"record not exists","errTable":"%s","errRecordCount":"%s"}';
  public $errParamNotFound = '{"error":"param not found","errParam":"%s"}';
  public $errGettingData = '{"error": "An error has occurred getting data"}';
  
  // authorized functions
  public function isCorrectFunc($func) {      
    return in_array($func, array('getCalendars',
                                 'delCalendar',
                                 'addCalendar',
                                 'editCalendar'
                                )
                   );
  }
}
