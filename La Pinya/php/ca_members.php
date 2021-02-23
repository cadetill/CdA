<?php

/**
 * class for members management
 *
 * @author cadetill
 * @see ca_base
 */
class ca_members extends ca_base {
  
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
      case 'getMembers':
        return $this->getMembers();
      case 'addMember':
        return $this->addMember();
      case 'delMember':
        return $this->delMember();
      case 'editMember':
        return $this->editMember();
      default :
        return $this->stringReplace($this->conf->errFuncNotFound, array($this->func));
    }
  }

  /*****************************************************************************
    PRIVATE FUNCTIONS
  *****************************************************************************/
  /**
   * returns the associated members to the APP
   * 
   * @return array with the records selected or string with the error that has occurred
   */
  private function getMembers() {
    // connect to database
    $idcon = $this->getConnection();
    if ($this->error <> '') {
      return $this->error;
    }

    // query to execute
    $sql = 'select * from '.$this->conf->dbprefix.'socis order by nom';
    $res = $idcon->query( $sql );
    
    // prepare result to return
    $ret = array();
    while($row = $res->fetch_assoc()) {
      //for every member we get his roles
      $sql = 'select rs.id, r.descrip '
           . 'from '.$this->conf->dbprefix.'rols_socis rs '
           . '  inner join '.$this->conf->dbprefix.'rols r on r.id = rs.id_rol '
           . 'where rs.id_soci = '.$row['id'];
      $res1 = $idcon->query( $sql );
      $rols = array();
      while($row1 = $res1->fetch_assoc()) {
        $rols[] = $row1;
      }
      $row['Rols'] = $rols;
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
   * adds a new record to ca_socis table
   * 
   * @param nom string with the name of member
   * @return array with records inserted or string with the error that has occurred
   */
  private function addMember() {
    // get input variables and check them
    if (filter_input(INPUT_SERVER, 'REQUEST_METHOD') === 'POST') {
      $request = json_decode(file_get_contents("php://input"), true);
      
      $nom = $this->normalizeParam($request['nom']);
    }
    else {
      $nom = $this->normalizeParam(filter_input(INPUT_GET, 'nom'));
     }
    
    if ($nom == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'nom');
    }

    // connect to database
    $idcon = $this->getConnection();
    if ($this->error <> '') {
      return $this->error;
    }

    // find record
    $sql = 'select * from '.$this->conf->dbprefix.'socis where nom = \''.$nom.'\'';
    $res = $idcon->query( $sql );
    
    $rows = $res->num_rows;
    
    // if record exists, error
    if ($rows != 0) {
      $ret = $this->stringReplace($this->conf->errRecordExists, array('socis', $rows));
    } 
    else {
      // add new record
      $sql = 'insert into '.$this->conf->dbprefix.'socis (nom) values (\''.$nom.'\')';
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
   * deletes a specified record from ca_socis
   * 
   * @param id socis id
   * @return array with the records deleted or string with the error that has occurred
   */
  private function delMember() {
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
    $sql = 'delete from '.$this->conf->dbprefix.'socis where id = \''.$id.'\'';
    $idcon->query( $sql );
    $res1 = $idcon->affected_rows;
    
    // close connection
    $idcon->close();
    
    // returns result
    return array(
                 'master' => $res1,
                 'id' => $id
                );
  }
  
  /*****************************************************************************
  /**
   * modifies an existing record
   * 
   * @param id string with the id of member to modify
   * @param nom string with member name
   * @param malnom string with nickname
   * @param data_naix string born date
   * @param data_alta string discharge date
   * @param data_baixa string leaving date
   * @param mail string member's email
   * @param mobil string mobile phone
   * @param is_casteller string Indicates if the member is a casteller or not.
   * @param is_graller string Indicates if the member is a graller or not.
   * @param sexe string member's sex 
   * @param alt_esp string Height at shoulder level
   * @param alt_ma string Height with arms extended up.
   * @param adresa string address
   * @param poblacio string city
   * @param cp string postal code
   * @param pais string country
   * @return array with records modifieds or string with the error that has occurred
   */
  private function editMember() {
    // get input variables and check them
    if (filter_input(INPUT_SERVER, 'REQUEST_METHOD') === 'POST') {
      $request = json_decode(file_get_contents("php://input"), true);
      
      $id = $this->normalizeParam($request['id']);
      $nom = $this->normalizeParam($request['nom']);
      $malnom = $this->normalizeParam($request['malnom']);
      $data_naix = $this->normalizeParam($request['data_naix']);
      $data_alta = $this->normalizeParam($request['data_alta']);
      $data_baixa = $this->normalizeParam($request['data_baixa']);
      $mail = $this->normalizeParam($request['mail']);
      $mobil = $this->normalizeParam($request['mobil']);
      $is_casteller = $this->normalizeParam($request['is_casteller']);
      $is_graller = $this->normalizeParam($request['is_graller']);
      $sexe = $this->normalizeParam($request['sexe']);
      $alt_esp = $this->normalizeParam($request['alt_esp']);
      $alt_ma = $this->normalizeParam($request['alt_ma']);
      $adresa = $this->normalizeParam($request['adresa']);
      $poblacio = $this->normalizeParam($request['poblacio']);
      $cp = $this->normalizeParam($request['cp']);
      $pais = $this->normalizeParam($request['pais']);
    }
    else {
      $id = $this->normalizeParam(filter_input(INPUT_GET, 'id'));
      $nom = $this->normalizeParam(filter_input(INPUT_GET, 'nom'));
      $malnom = $this->normalizeParam(filter_input(INPUT_GET, 'malnom'));
      $data_naix = $this->normalizeParam(filter_input(INPUT_GET, 'data_naix'));
      $data_alta = $this->normalizeParam(filter_input(INPUT_GET, 'data_alta'));
      $data_baixa = $this->normalizeParam(filter_input(INPUT_GET, 'data_baixa'));
      $mail = $this->normalizeParam(filter_input(INPUT_GET, 'mail'));
      $mobil = $this->normalizeParam(filter_input(INPUT_GET, 'mobil'));
      $is_casteller = $this->normalizeParam(filter_input(INPUT_GET, 'is_casteller'));
      $is_graller = $this->normalizeParam(filter_input(INPUT_GET, 'is_graller'));
      $sexe = $this->normalizeParam(filter_input(INPUT_GET, 'sexe'));
      $alt_esp = $this->normalizeParam(filter_input(INPUT_GET, 'alt_esp'));
      $alt_ma = $this->normalizeParam(filter_input(INPUT_GET, 'alt_ma'));
      $adresa = $this->normalizeParam(filter_input(INPUT_GET, 'adresa'));
      $poblacio = $this->normalizeParam(filter_input(INPUT_GET, 'poblacio'));
      $cp = $this->normalizeParam(filter_input(INPUT_GET, 'cp'));
      $pais = $this->normalizeParam(filter_input(INPUT_GET, 'pais'));
    }
    
    if (($id == '') || (!is_numeric($id))) {
      return $this->stringReplace($this->conf->errParamNotFound, 'id');
    }
    if ($nom == '') {
      return $this->stringReplace($this->conf->errParamNotFound, 'nom');
    }

    // connect to database
    $idcon = $this->getConnection();
    if ($this->error <> '') {
      return $this->error;
    }

    // find record
    $sql = 'select * from '.$this->conf->dbprefix.'socis where id = '.$id;
    $res = $idcon->query( $sql );
    
    $rows = $res->num_rows;
    
    // if not record exists, error
    if ($rows == 0) {
      $ret = $this->stringReplace($this->conf->errRecordNotExists, array('calendaris', $rows));
    } 
    else {
      // edit record
      $sql = 'update '.$this->conf->dbprefix.'socis set idcalendar = \''.$idcalendar.'\', `key` = \''.$key.'\', nom = \''.$nom.'\', nom_curt = \''.$nom_curt.'\' where id = '.$id;
      $res = $idcon->query( $sql );
      $ret = array(
                   'master' => $res->num_rows,
                   'id' => $id
                  );
    }
    return $ret;
  }
  
  
}
