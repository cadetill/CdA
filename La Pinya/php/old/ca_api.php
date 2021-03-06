<?php
  /*********************************************************/
  /******************* inicializaciones ********************/
  /*********************************************************/
  // inicializaciones generales
  define( 'cda_check', 1 );     // definim la variable cda_check per a indicar que arranquem correctament
  $strTime = microtime();       // per si volem saber quan hem trigat en generar la pàgina

  // carreguem config...
  require_once( dirname(__FILE__).'/ca_config.php' );
  
  // carreguem APIs particulars
  require_once( dirname(__FILE__).'/ca_api_calendar.php' );
  require_once( dirname(__FILE__).'/ca_api_socis.php' );
  require_once( dirname(__FILE__).'/ca_api_news.php' );
  require_once( dirname(__FILE__).'/ca_api_rols.php' );

  // conectem amb la base de dades 
  require_once( path_source.'/class_conection.php' );
  $bd_connection = new TConection( db_dbname, db_server, db_user, db_pass );
  
  if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $_REQUEST = json_decode(file_get_contents("php://input"), true);
  }
  
  $text = implode(" ",$_REQUEST);
  $sql = 'insert into '.db_prefix.'test (datea, dir, test) values (\''.date("Y-m-d H:i:s").'\', \'in\', \''.$text.'\')';
  $bd_connection->execSQL($sql);

  if (!isset($_REQUEST['func'])) {
    return_emty_json($bd_connection, '', false);
	exit;
  }
  
  if ($_REQUEST['func'] != 'get_sql') {
    $func = $_REQUEST['func'];
	
	if (function_exists($func)) {
      $func($bd_connection, $_REQUEST);
    } 
    else {
      return_emty_json($bd_connection, '', false);
    }
  } 
  else {
    get_sql($bd_connection, $_REQUEST['sql'], true, true); 
  }
  $bd_connection->closeConnection(); 

  /*******************************************************************************************************************************************************************
    funció que retorna un JSON amb un determinat status i/o l'id del darrer registre afegit
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $value => indica si s'ha de retornar un "ok" o ""
      - $lastInsert => indica si s'ha de retornar el darrer ID insertat
  *******************************************************************************************************************************************************************/
  function return_emty_json($bd_connection, $value, $lastInsert) {
	$lastId = '';
	if ($lastInsert) {
      $lastId = ', '.$bd_connection->getLastId().' id';
    }
    
	if ($value == '') {
      $sql = 'select status'.$lastId.' from '.db_prefix.'status where status = \'\'';
	} 
	else {
      $sql = 'select status'.$lastId.' from '.db_prefix.'status where status = \'ok\'';
	}
    
    get_sql($bd_connection, $sql, false, true);
  }

  /*******************************************************************************************************************************************************************
    funció que retorna un JSON amb un determinat status i el nombre de registres afectats en la darrera operació d'insert/update/delete
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $status => indica si s'ha de retornar un "ok" o ""
      - $lastInsert => indica si s'ha de retornar el darrer ID insertat
  *******************************************************************************************************************************************************************/
  function return_affectedRows_json($bd_connection, $status, $lastInsert, $res, &$lastId = "") {
    $lastId = $bd_connection->getLastId();
    if ($lastInsert) {
      $Id = ', '.$bd_connection->getLastId().' id';
    }
	
	$affected = ', '.$bd_connection->affectedRows().' affected';
    
	if ($status == '') {
      $sql = 'select "X" connected, status'.$Id.$affected.' from '.db_prefix.'status where status = \'\'';
	} 
	else {
      $sql = 'select "X" connected, status'.$Id.$affected.' from '.db_prefix.'status where status = \'ok\'';
	}

    get_sql($bd_connection, $sql, false, true);
  }

  /*******************************************************************************************************************************************************************
    funció que executa una sentencia SQL pasada per paràmetre i, si s'escau, retorna un JSON amb les files
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $sql => sentència SQL a executar. Si la sentència no és de selecció, retornarà un status + ID. En cas de selecció, retornarà els registres resultants
      - $control => indica si s'ha de fer un control de la sentència SQL per evitar sentències que no siguin de select no desitjades
      - $withjson => indica si el resultat de la sentència cal que retorni un JSON o no
  *******************************************************************************************************************************************************************/
  function get_sql($bd_connection, $sql, $control, $withjson, &$lastId = "") {
    // comprovem que la primera instrucció sigui SELECT per evitar insert, update, delete,.... no autoritzats
    $arr = explode(' ', trim($sql));
    if (($control) && (strtolower($arr[0]) != 'select')) {
      if ($withjson == true)
        return_emty_json($bd_connection, '', false);
    } 
    else {
      // si tot va bé, fem sql i el retornem 
      $res = $bd_connection->execSQL($sql);
      
      // si hi ha error en SQL, sortim
      if ($res == false) {
        echo $bd_connection->lastError().'<br>';
        echo $sql;
        return;
      }
      
      // si no volem resposta JSON sortim
      if ($withjson == false) {
        return $res;
      }
      
      // mirem si NO és una sentència select
      if (strtolower($arr[0]) != 'select') {
        $val = '';
        if ($res) $val = 'X';
        
        if (strtolower($arr[0]) == 'insert') {
          //return_emty_json($bd_connection, $val, true);
          return_affectedRows_json($bd_connection, $val, true, $res, $lastId);
        }
        else {
          //return_emty_json($bd_connection, $val, false);
		  //echo $sql.'<br>'.$val.'<br>';
          return_affectedRows_json($bd_connection, $val, true, $res);
        }
      }
      else {  // és una sentència select
        $row_set = [];
        while($row = $res->fetch_assoc()) {
          foreach ($row as $k => $v) {
            $row[$k] = $v;
          }
          $row_set[] = $row;
        }
        // retornem el JSON
		$sql = "insert into ".db_prefix."test (datea, dir, test) values ('".date("Y-m-d H:i:s")."', 'out', '".str_replace("'", "´", json_encode($row_set, JSON_UNESCAPED_UNICODE))."')";
        $bd_connection->execSQL($sql);
        echo json_encode($row_set, JSON_UNESCAPED_UNICODE);
      }
    }
  }

  /*******************************************************************************************************************************************************************
    funció que normalitza un paràmetre
    Paràmetres:
      - $param => string a normalitzar
  *******************************************************************************************************************************************************************/
  function NormalizeParam($param) {
    $param = str_replace('%20', ' ', $param);
    //$param = str_replace('%%%', "'", $param);
    return $param;
  }
?>