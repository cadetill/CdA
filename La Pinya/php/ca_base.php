<?php

/**
 * base class for all REST classes
 *
 * @author cadetill
 */
class ca_base {
  protected $conf;  // object from ca_config class
  protected $error; // last error
  protected $func; // function to execute

  /**
   * class constructor
   * 
   * @param $conf object from ca_config class
   * @param $func fuction to execute
   */
  public function __construct($conf, $func) {
    $this->conf = $conf;
    $this->error = '';
    $this->func = $func;
  }

  /****************************************************
    protectec methods
  ****************************************************/
  /**
   * establishes a connection to the mySQL database
   * 
   * @return a mysqli object connection
   */
  protected function getConnection() {
    $this->error = '';
    $idcon = new mysqli($this->conf->dbserver, $this->conf->dbuser, $this->conf->dbpas, $this->conf->dbnamebd);
    if ($idcon->connect_error) {
      $this->conf->errGettingData = 'An error has occurred getting data.';
      exit;
    }
    $idcon->query("SET NAMES 'utf8'");
    
    return $idcon;
  }
  
  /**
   * change some characters for others
   * 
   * @param $param string
   * @return string
   */
  protected function normalizeParam($param) {
    $p = str_replace('%20', ' ', $param);
    return $p;
  }
  
  /**
   * replace the %s occurrences into the $text string by the $arr string
   * 
   * @param $text string that contains %s
   * @param $arr string or array of strings to replace the %s
   * @return string
   */
  public function stringReplace($text, $arr = '') {
    if (is_array($arr)) {
      foreach ($arr as $var) {
        $text = preg_replace('/%s/', $var, $text, 1);
      }
    }
    else {
      $text = str_replace('%s', $arr, $text);
    }
    return $text;
  }

}
