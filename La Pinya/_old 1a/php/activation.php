<?php
  // inicializaciones generales
  define( 'cda_check', 1 );     // definim la variable cda_check per a indicar que arranquem correctament

  // carreguem config...
  require_once( dirname(__FILE__).'/ca_config.php' );
  
  // conectem amb la base de dades 
  require_once( path_source.'/class_conection.php' );
  $bd_connection = new TConection( db_dbname, db_server, db_user, db_pass );
  
  $sql = 'select * from '.db_prefix.'users where codact = \''.$_REQUEST['id'].'\'';
  $res = $bd_connection->execSQL($sql);
  
  $rows = $bd_connection->numRows($res);
  if ($rows != 0) {
    $sql = 'update '.db_prefix.'users set active = 1 where codact = \''.$_REQUEST['id'].'\'';
    $res = $bd_connection->execSQL($sql);
	echo "activació correcta. Ja pots accedir a l'aplicació";
  }
  else {
	echo "usuari innexisten";
  }
  
  $bd_connection->closeConnection;
?>