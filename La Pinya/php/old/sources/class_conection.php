<?php
  if ( !defined('cda_check') )
    die('Hacking attempt...');
	
/****************************************************************************************************
 ***************************     Clase de conexión a la base de datos     ***************************
 ****************************************************************************************************
 * TConexion: Constructor de la clase
 * void show_sql_error(): (private)
 * void show_db_error(): (private)
 * int getIdcon(): (public) 
 * int execSQL( $sql ): (public)
 * int numRows($res): (public)
 * int numSqls(): (public)
 * void closeQuery($res): (public)
 * void closeConnection(): (public) 
 ***************************************************************************************************/
class TConection {
  var $server;
  var $user;
  var $pas;
  var $idcon;
  var $namebd;
  var $countsql;
  var $showError;

  /**************************************************************************************************
   * funciones de carácter privado
   *************************************************************************************************/
  /**************************************************************************************************
   * @visibility private
   * @param $sql string
   * @description => si hay un problema de SQL, muestra un error
   *************************************************************************************************/
  function show_sql_error( $sql ) {
    if ( $this->showError ) {
      global $webmaster_email;

      // No queremos que esta página se guarde
      header( 'Expires: Mon, 26 Jul 1997 05:00:00 GMT' );
      header( 'Last-Modified: '.gmdate('D, d M Y H:i:s').' GMT' );
      header( 'Cache-Control: no-cache' );
	
      // enviamos correo al webmaster
      $texto = "Sentencia SQL errónea!\n\n" .
               "Mensaje de error de MySQL: \n" .
               ($mysql_error == '' ? '' : $mysql_error) . "\n\n" .
               "Esto es un correo de notificación para hacerte saber que se ha producido una sentencia SQL errónea.\n\n" .
               "   * Sentencia errónea: " . $sql . "\n\n" .
               "   * Documento: " . __FILE__ . "\n\n" .
               "   * SERVER['PHP_SELF']: " . $_SERVER['PHP_SELF'] . "\n\n" .
               "   * SERVER['HTTP_REFERER']: " . $_SERVER['HTTP_REFERER'] . "\n\n" .
               "   * SERVER['HTTP_USER_AGENT']: " . $_SERVER['HTTP_USER_AGENT'] . "\n\n" .
               "   * SERVER['REMOTE_ADDR']: " . $_SERVER['REMOTE_ADDR'] . "\n\n" .
               "   * SERVER['REMOTE_HOST']: " . $_SERVER['REMOTE_HOST'] . "\n\n";
      @mail( $webmaster_email, 'Error de SQL!', $texto );

      // creamos pantalla de error de conexión
      echo '
        <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
        <html>
          <head>
            <title>Sentencia errónea</title>
          </head>
          <body>
            <h3>Sentencia err&oacute;nea</h3>
            Lo sentimos, pero se ha producido una sentencia SQL err&oacute;nea. Por favor, si el problema continua, p&oacute;ngase en contacto con un administrador para que miren de solventar el problema. Gracias.
          </body>
        </html>';
  
      die;
	}
  }

  /**************************************************************************************************
   * @visibility private
   * @description => si hay problemas de conexión, se llamará a este método
   *************************************************************************************************/
  function show_db_error() {
    if ( $this->showError ) {
      global $webmaster_email;

      // No queremos que esta página se guarde
      header( 'Expires: Mon, 26 Jul 1997 05:00:00 GMT' );
      header( 'Last-Modified: '.gmdate('D, d M Y H:i:s').' GMT' );
      header( 'Cache-Control: no-cache' );
	
      // enviamos correo al webmaster
      $texto = "'Hay un problema con la base de datos!'\n\n" .
               "Mensaje de error de MySQL: \n" .
               ($mysql_error == '' ? '' : $mysql_error) . "\n\n" .
               "Esto es un correo de notificación para hacerte saber que no se puede conectar con la base de datos.\n" .
               "Contanta con tu proveedor de servicios si el problema continua.\n\n" .
               "   * Documento: " . __FILE__ . "\n\n" .
               "   * SERVER['PHP_SELF']: " . $_SERVER['PHP_SELF'] . "\n\n" .
               "   * SERVER['HTTP_REFERER']: " . $_SERVER['HTTP_REFERER'] . "\n\n" .
               "   * SERVER['HTTP_USER_AGENT']: " . $_SERVER['HTTP_USER_AGENT'] . "\n\n" .
               "   * SERVER['REMOTE_ADDR']: " . $_SERVER['REMOTE_ADDR'] . "\n\n" .
               "   * SERVER['REMOTE_HOST']: " . $_SERVER['REMOTE_HOST'] . "\n\n";
      @mail( $webmaster_email, 'CdApp: Error de base de datos!', $texto );

      // creamos pantalla de error de conexión
      echo '
        <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
        <html>
          <head>
            <title>Problemas de conexión</title>
          </head>
          <body>
            <h3>Problemas de conexi&oacute;n</h3>
            Lo sentimos, tenemos problemas de conexi&oacute;n a la base de datos. Podr&iacute;a ser que el servidor estubiera saturado. Por favor, int&eacute;ntalo de nuevo m&aacute;s tarde.
          </body>
        </html>';
  
      die;
	}
  }

  /**************************************************************************************************
   * funciones de carácter protegido
   *************************************************************************************************/
  
  /**************************************************************************************************
   * funciones de carácter public y constructores
   *************************************************************************************************/
  /**************************************************************************************************
   * @visibility public
   * @param $namebd string
   * @param $server string
   * @param $user string
   * @param $pas string
   * @description => constructor de la clase. Se encarga de conectarse a la BD
   *************************************************************************************************/
  function TConection( $namebd, $server, $user, $pas, $showError = true ) {
    // inicializamos variables
    $this->server = $server;
    $this->user = $user;
    $this->pas = $pas;
    $this->namebd = $namebd; 
    $this->countsql = 0;
    $this->showError = $showError;
    // conectamos al servidor MySQL
    $this->idcon = new mysqli($this->server, $this->user, $this->pas, $this->namebd);
    $this->execSQL("SET NAMES 'utf8'");
	if (($this->idcon->connect_error) && ($this->showError)) {
      die('Error de Conexión (' . $this->idcon->connect_errno . ') ' . $this->idcon->connect_error);
    }
  }
  
  /**************************************************************************************************
   * @visibility public
   * @return string
   * @description => retorna el identificador de la conexión
   *************************************************************************************************/
  function getIdcon() {
    return $this->idcon;
  }
  
  /**************************************************************************************************
   * @visibility public
   * @return objeto mysqli_result
   * @param $sql string
   * @param $showError string
   * @description => ejecuta la sentencia SQL $sql y retorna un puntero a la misma
   *************************************************************************************************/
  function execSQL( $sql, $showError = '' ) {
    $res = $this->idcon->query( $sql );
//    echo 'sql: '.$sql.'<br>';
//    echo 'affected_rows: '.$this->idcon->affected_rows.'<br>';
//	$res = $this->idcon->query( 'select * from ca_calendaris where id = 21' );
//    echo 'sql: '.'select * from ca_calendaris where id = 21'.'<br>';
//    echo 'affected_rows: '.$this->idcon->affected_rows.'<br>';
    if ($this->idcon->error) {
      return false;
    } 
    else {
      return $res;
    }
  }
  
  /**************************************************************************************************
   * @visibility public
   * @return string
   * @description => devuelve el últime error de una sentencia SQL
   *************************************************************************************************/
  function lastError() {
    return $this->idcon->error;
  }
  
  /**************************************************************************************************
   * @visibility public
   * @return int
   * @description => devuelve el número de filas afectadas por una sentencia DELETE, INSERT, o UPDATE cuyo identificador es $res
   *************************************************************************************************/
  function affectedRows() {
    return $this->idcon->affected_rows;
  }
  
  /**************************************************************************************************
   * @visibility public
   * @return int
   * @param $res objeto mysqli_result
   * @description => retorna el número de filas de una sentecia SQL cuyo identificador es $res
   *************************************************************************************************/
  function numRows( $res ) {
    return $res->num_rows;
  }

  /**************************************************************************************************
   * @visibility public
   * @return int
   * @description => retorna el número de SQLs lanzados hasta el momento
   *************************************************************************************************/
  function numSqls() {
    return $this->countsql;
  }

  /**************************************************************************************************
   * @visibility public
   * @param $res objeto mysqli_result
   * @description => cierra la sentencia SQL lanzada
   *************************************************************************************************/
  function closeQuery($res) {
    $res->close();
  }

  /**************************************************************************************************
   * @visibility public
   * @description => cierra la sentencia SQL lanzada
   *************************************************************************************************/
  function closeConnection() {
    $this->idcon->close();
  }

  /**************************************************************************************************
   * @visibility public
   * @return string
   * @description => devuelve el charset de la conexión
   *************************************************************************************************/
  function getCharacterSet() {
    return $this->idcon->character_set_name();
  }

  /**************************************************************************************************
   * @visibility public
   * @param $charset character set a aplicar
   * @description => establece el character set de la conexión
   *************************************************************************************************/
  function setCharacterSet($charset) {
    $this->idcon->set_charset($charset);
  }

  /**************************************************************************************************
   * @visibility public
   * @return integer
   * @description => devuelve el id autogenerado que se utilizó en la última consulta
   *************************************************************************************************/
  function getLastId() {
    return $this->idcon->insert_id;
  }
}
?>