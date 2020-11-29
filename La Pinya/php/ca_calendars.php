<?php

/**
 * class for calendars management
 *
 * @author cadetill
 * @see ca_base
 */
class ca_calendars extends ca_base {
  
  /**
   * class constructor
   * 
   * @param $conf object from ca_config class
   * @param $func function to execute
   * @param $params parameters needed to do the operations
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
      case 'getCalendars':
        return $this->getCalendars();
      case 'addCalendar':
        return $this->addCalendar();
      case 'delCalendar':
        return $this->delCalendar();
      case 'editCalendar':
        return $this->editCalendar();
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
  private function getCalendars() {
    // connect to database
    $idcon = $this->getConnection();
    if ($this->error <> '') {
      return $this->error;
    }

    // execute query
    $sql = 'select * from '.$this->conf->dbprefix.'calendaris order by nom';
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
  
  /*****************************************************************************
  /**
   * deletes a specified record from ca_calendar and theirs ca_events.
   * 
   * @param id calendar id
   * @return array with the records deleted or string with the error that has occurred
   */
  private function delCalendar() {
    // get input variable and check it
    if (filter_input(INPUT_SERVER, 'REQUEST_METHOD') === 'POST') {
      $request = json_decode(file_get_contents("php://input"), true);
      $id = $request['id'];
    }
    else {
      $id = filter_input(INPUT_GET, 'id');
    }

    if (!is_numeric($id)) {
      return $this->stringReplace($this->conf->errBadParameter, 'id');
    }

    // connect to database
    $idcon = $this->getConnection();
    if ($this->error <> '') {
      return $this->error;
    }

    // execute query
    $sql = 'delete from '.$this->conf->dbprefix.'calendaris where id = \''.$id.'\'';
    $idcon->query( $sql );
    $res1 = $idcon->affected_rows;

    $sql = 'delete from '.$this->conf->dbprefix.'events where id_cal = \''.$id.'\'';
    $idcon->query( $sql );
    $res2 = $idcon->affected_rows;
    
    // close connection
    $idcon->close();
    
    // returns result
    return array(
                 'master' => $res1,
                 'detail01' => $res2,
                 'id' => $id
                );
  }
  
  /*****************************************************************************
  /**
   * adds a new record to ca_calendar table
   * 
   * @param idcalendar string identifier from Google
   * @param key string key from Google to access calendar
   * @param nom string name to assign to calendar
   * @param nom_curt string small name to assign to calendar
   * @return array with records inserted or string with the error that has occurred
   */
  private function addCalendar() {
    // get input variables and check them
    if (filter_input(INPUT_SERVER, 'REQUEST_METHOD') === 'POST') {
      $request = json_decode(file_get_contents("php://input"), true);
      
      $idcalendar = $this->normalizeParam($request['idcalendar']);
      $key = $this->normalizeParam($request['key']);
      $nom = $this->normalizeParam($request['nom']);
      $nom_curt = $this->normalizeParam($request['nom_curt']);
    }
    else {
      $idcalendar = $this->normalizeParam(filter_input(INPUT_GET, 'idcalendar'));
      $key = $this->normalizeParam(filter_input(INPUT_GET, 'key'));
      $nom = $this->normalizeParam(filter_input(INPUT_GET, 'nom'));
      $nom_curt = $this->normalizeParam(filter_input(INPUT_GET, 'nom_curt'));
    }
    
    if ($idcalendar == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'idcalendar');
    }
    if ($key == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'key');
    }
    if ($nom == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'nom');
    }
    if ($nom_curt == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'nom_curt');
    }

    // connect to database
    $idcon = $this->getConnection();
    if ($this->error <> '') {
      return $this->error;
    }

    // find record
    $sql = 'select * from '.$this->conf->dbprefix.'calendaris where idcalendar = \''.$idcalendar.'\'';
    $res = $idcon->query( $sql );
    
    $rows = $res->num_rows;
    
    // if record exists, error
    if ($rows != 0) {
      $ret = $this->stringReplace($this->conf->errRecordExists, array('calendaris', $rows));
    } 
    else {
      // add new record
      $sql = 'insert into '.$this->conf->dbprefix.'calendaris (idcalendar, `key`, nom, nom_curt) values (\''.$idcalendar.'\', \''.$key.'\', \''.$nom.'\', \''.$nom_curt.'\')';
      $res = $idcon->query( $sql );
      $ret = array(
                   'master' => $res->num_rows,
                   'id' => $idcon->insert_id
                  );
    }
    return $ret;
  }

  /*****************************************************************************
  /**
   * modifies an existing record
   * 
   * @param id string with the id of calendar to modify
   * @param idcalendar string identifier from Google
   * @param key string key from Google to access calendar
   * @param nom string name to assign to calendar
   * @param nom_curt string small name to assign to calendar
   * @return array with records modifieds or string with the error that has occurred
   */
  private function editCalendar() {
    // get input variables and check them
    if (filter_input(INPUT_SERVER, 'REQUEST_METHOD') === 'POST') {
      $request = json_decode(file_get_contents("php://input"), true);
      
      $id = $this->normalizeParam($request['id']);
      $idcalendar = $this->normalizeParam($request['idcalendar']);
      $key = $this->normalizeParam($request['key']);
      $nom = $this->normalizeParam($request['nom']);
      $nom_curt = $this->normalizeParam($request['nom_curt']);
    }
    else {
      $id = $this->normalizeParam(filter_input(INPUT_GET, 'id'));
      $idcalendar = $this->normalizeParam(filter_input(INPUT_GET, 'idcalendar'));
      $key = $this->normalizeParam(filter_input(INPUT_GET, 'key'));
      $nom = $this->normalizeParam(filter_input(INPUT_GET, 'nom'));
      $nom_curt = $this->normalizeParam(filter_input(INPUT_GET, 'nom_curt'));
    }
    
    if (($id == '') || (!is_numeric($id))) {
      return $this->stringReplace($this->conf->errParamNotFound, 'id');
    }
    if ($idcalendar == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'idcalendar');
    }
    if ($key == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'key');
    }
    if ($nom == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'nom');
    }
    if ($nom_curt == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'nom_curt');
    }

    // connect to database
    $idcon = $this->getConnection();
    if ($this->error <> '') {
      return $this->error;
    }

    // find record
    $sql = 'select * from '.$this->conf->dbprefix.'calendaris where id = '.$id;
    $res = $idcon->query( $sql );
    
    $rows = $res->num_rows;
    
    // if not record exists, error
    if ($rows == 0) {
      $ret = $this->stringReplace($this->conf->errRecordNotExists, array('calendaris', $rows));
    } 
    else {
      // edit record
      $sql = 'update '.$this->conf->dbprefix.'calendaris set idcalendar = \''.$idcalendar.'\', `key` = \''.$key.'\', nom = \''.$nom.'\', nom_curt = \''.$nom_curt.'\' where id = '.$id;
      $res = $idcon->query( $sql );
      $ret = array(
                   'master' => $res->num_rows,
                   'id' => $id
                  );
    }
    return $ret;
  }

}
