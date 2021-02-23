<?php

/**
 * class for roles management
 *
 * @author cadetill
 * @see ca_base
 */
class ca_roles extends ca_base { 
  
  /**
   * class constructor
   * 
   * @param $conf object from ca_config class
   * @param $func function to execute
   */
  public function __construct($conf, $func) {
    parent::__construct($conf, $func);
  }

  /*****************************************************************************
    PUBLIC FUNCTIONS
  *****************************************************************************/
  /**
   * execute $this->func if can be executed
   * 
   * @return array with information to return or string with error
   */
  public function execute() {
    switch ($this->func) {
      case 'getRoles':
        return $this->getRoles();
      case 'addRole':
        return $this->addRole();
      case 'delRole':
        return $this->delRole();
      case 'editRole':
        return $this->editRole();
      default :
        return $this->stringReplace($this->conf->errFuncNotFound, array($this->func));
    }
  }

  /*****************************************************************************
    PRIVATE FUNCTIONS
  *****************************************************************************/
  /**
   * returns the associated calendars to the APP
   * 
   * @return array with the records selected or string with the error that has occurred
   */
  private function getRoles() {
    // connect to database
    $idcon = $this->getConnection();
    if ($this->error <> '') {
      return $this->error;
    }

    // query to execute
    $sql = 'select * from '.$this->conf->dbprefix.'rols order by descrip';
    $res = $idcon->query( $sql );
    
    // prepare result to return
    $ret = array();
    while($row = $res->fetch_assoc()) {
      $ret[] = $row;
    }

    // save query if debud mode is active
    if ($this->conf->debugMode) {
      $sql = "insert into ".$this->conf->dbprefix."test (datea, dir, test) values ('".date("Y-m-d H:i:s")."', 'out', '".str_replace("'", "´", json_encode($ret, JSON_UNESCAPED_UNICODE))."')";
      $idcon->query( $sql );
    }
    
    // close connection
    $idcon->close(); 
 
    return $ret;
  }

}
