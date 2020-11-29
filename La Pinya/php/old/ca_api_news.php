<?php
  /*******************************************************************************************************************************************************************
    Funció que recupera les noticies.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * start: registre a partir del qual volem recuperar les noticies
                   * count: quantitat de noticies a recuperar
                   * iscomunica: indica si l'usuari és administracor ("X") o no ("")
  *******************************************************************************************************************************************************************/
  function GetNews($bd_connection, $params) {
    // si no hi ha cap noticia, afegim la inicial
    $date = date("Y-m-d");
	$hora = date("H:i:s");
    $sql = 'select count(*) total from '.db_prefix.'noticies ';
    $res = get_sql($bd_connection, $sql, false, false);
    $row = $res->fetch_assoc();
	
    if ($row['total'] == 0) {
      $data = [
               "title" => "Benvingut/da!",
               "body" => "Amb aquesta aplicació podràs confirmar assistències tant a assaig, com a diades o esdeveniments, així com consultar les pinyes i troncs o estar al corrent de tot allò important per a la colla.",
               "pubdate" => $date,
               "hora" => $hora,
               "user" => "0",
               "json" => ""
              ];
      AddNews($bd_connection, $data);
    }
    
    // retornem les notícies
    if ($params['iscomunica'] == 'X') {
      $sql  = 'SELECT * FROM '.db_prefix.'noticies order by publicar desc LIMIT '.$params['start'].','.$params['count'];
    }
    else {
      $sql  = 'SELECT * FROM '.db_prefix.'noticies where publicar <= \''.$date.'\' order by publicar desc LIMIT '.$params['start'].','.$params['count'];
    }
    $res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que afegeix una notícia.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * title: títol de la notícia
                   * body: cos de la notícia
                   * pubdate: data de publicació de la notícia
                   * hora: hora de publicació de la notícia
                   * user: usuari que dóna d'alta la notícia
                   * json: si volen retorn de json ("X") o no ("")
  *******************************************************************************************************************************************************************/
  function AddNews($bd_connection, $params) {
    $params['title'] = NormalizeParam($params['title']);
    $params['body'] = NormalizeParam($params['body']);

    $date = date("Y-m-d H:i:s");
    $json = $params['json'] != "";
    $sql  = "INSERT INTO ".db_prefix."noticies (titol, contingut, hora, usera, datea, publicar) ";
    $sql .= "VALUES ('".$params['title']."', '".$params['body']."', '".$params['hora']."', '".$params['user']."', '".$date."', '".$params['pubdate']."')";
    get_sql($bd_connection, $sql, false, $json);
  }

  /*******************************************************************************************************************************************************************
    Funció que esborrarà una notícia.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * id: id de la notícia
  *******************************************************************************************************************************************************************/
  function DelNews($bd_connection, $params) {
    $sql  = 'delete from '.db_prefix.'noticies where id = '.$params['id'];
    get_sql($bd_connection, $sql, false, false);
    
    return_emty_json($bd_connection, 'ok', true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que modifica una notícia.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * id: id de la notícia
                   * title: títol de la notícia
                   * body: cos de la notícia
                   * pubdate: data de publicació de la notícia
                   * hora: hora de publicació de la notícia
                   * user: usuari que dóna d'alta la notícia
  *******************************************************************************************************************************************************************/
  function EditNews($bd_connection, $params) {
    $params['title'] = NormalizeParam($params['title']);
    $params['body'] = NormalizeParam($params['body']);

    $date = date("Y-m-d H:i:s");
    $sql  = 'UPDATE '.db_prefix.'noticies ';
	$sql .= 'SET titol = \''.$params['title'].'\', contingut = \''.$params['body'].'\', publicar = \''.$params['pubdate'].'\', hora = \''.$params['hora'].'\', ';
	$sql .= '    usera = 0, datea = \''.$date.'\' ';
	$sql .= 'WHERE id = '.$params['id'];
	get_sql($bd_connection, $sql, false, true);
  }
?>