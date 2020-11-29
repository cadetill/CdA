<?php
  /*******************************************************************************************************************************************************************
    Funció que retorna els calendaris associats a la APP. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
  *******************************************************************************************************************************************************************/
  function GetCalendars($bd_connection) {
    $sql = 'select * from '.db_prefix.'calendaris';
    $res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que esborra una entrada a ca_calendar i els seus respectius ca_events.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * id: identificador del calendari a esborrar
  *******************************************************************************************************************************************************************/
  function DelCalendar($bd_connection, $params) {
    $sql = 'delete from '.db_prefix.'calendaris where id = \''.$params['id'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);

    $sql = 'delete from '.db_prefix.'events where id_cal = \''.$params['id'].'\'';
    $res1 = get_sql($bd_connection, $sql, false, false);
    
    return_affectedRows_json($bd_connection, 'ok', true, $res);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que afegeix un entrada a ca_calendar.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * idcalendar: identificador de Google del calendari 
                   * key: clau de Google per accedir al calendari
                   * nom: nom a assignar al calendari
                   * nom_curt: nom abreviat a assignar al calendari
  *******************************************************************************************************************************************************************/
  function AddCalendar($bd_connection, $params) {
    $params['idcalendar'] = NormalizeParam($params['idcalendar']);
    $params['key'] = NormalizeParam($params['key']);
    $params['nom'] = NormalizeParam($params['nom']);
    $params['nom_curt'] = NormalizeParam($params['nom']);
    $sql = 'select * from '.db_prefix.'calendaris where idcalendar = \''.$params['idcalendar'].'\' and "key" = \''.$params['key'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);
    
    $rows = $bd_connection->numRows($res);
    
    if ($rows != 0) {
      //get_sql($bd_connection, $sql, false, true);
	  return_affectedRows_json($bd_connection, 'X', true, $res);
    } 
    else {
      // afegim registre
      $sql = 'insert into '.db_prefix.'calendaris (idcalendar, `key`, nom, nom_curt) values (\''.$params['idcalendar'].'\', \''.$params['key'].'\', \''.$params['nom'].'\', \''.$params['nom_curt'].'\')';
      get_sql($bd_connection, $sql, false, true);
    }
  }
  
  /*******************************************************************************************************************************************************************
    Funció que modifica un calendari.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * id: id del calendari
                   * idcalendar: id de Google Calendar
                   * key: clau de Google Calendar
                   * nom: nom del calendari
                   * nom_curt: nom breu del calendari
  *******************************************************************************************************************************************************************/
  function EditCalendar($bd_connection, $params) {
    $params['idcalendar'] = NormalizeParam($params['idcalendar']);
    $params['key'] = NormalizeParam($params['key']);
    $params['nom'] = NormalizeParam($params['nom']);
    $params['nom_curt'] = NormalizeParam($params['nom_curt']);

    $sql  = 'UPDATE '.db_prefix.'calendaris ';
	$sql .= 'SET idcalendar = \''.$params['idcalendar'].'\', `key` = \''.$params['key'].'\', nom = \''.$params['nom'].'\', nom_curt = \''.$params['nom_curt'].'\' ';
	$sql .= 'WHERE id = '.$params['id'];
    get_sql($bd_connection, $sql, false, true);
  }
 
  /******************************************************************************************************************************************************************/
  /******************************************************************************************************************************************************************/
  /******************************************************************************************************************************************************************/
 
  /*******************************************************************************************************************************************************************
    Funció que retorna les temporades associats a la APP. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
  *******************************************************************************************************************************************************************/
  function GetTemporades($bd_connection) {
    $sql = 'select any, descripcio from '.db_prefix.'temporades order by any desc';
    $res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que esborra una temporada a ca_temporades i els seus respectius ca_events.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * any: identificador (any) de la temporada a esborrar
  *******************************************************************************************************************************************************************/
  function DelTemporada($bd_connection, $params) {
    $sql = 'delete from '.db_prefix.'temporades where any = \''.$params['any'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);

    $sql = 'delete from '.db_prefix.'events where any = \''.$params['any'].'\'';
    $res1 = get_sql($bd_connection, $sql, false, false);
    
    return_affectedRows_json($bd_connection, 'ok', true, $res);
  }
 
  /*******************************************************************************************************************************************************************
    Funció que afegeix una temporada.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * any: any de la temproada del calendari a crear
                   * descripcio: descripció per a la temporada
  *******************************************************************************************************************************************************************/
  function AddTemporada($bd_connection, $params) {
    $params['descripcio'] = NormalizeParam($params['descripcio']);
    $sql = 'select * from '.db_prefix.'temporades where any = '.$params['any'];
    $res = get_sql($bd_connection, $sql, false, false);
    
    $rows = $bd_connection->numRows($res);

    if ($rows != 0) {
      //get_sql($bd_connection, $sql, false, true);
      return_affectedRows_json($bd_connection, 'X', true, $res);
    } 
    else {
      // afegim registre
      $sql = 'insert into '.db_prefix.'temporades (any, descripcio) values ('.$params['any'].', \''.$params['descripcio'].'\')';
      get_sql($bd_connection, $sql, false, true);
    }
  }
  
  /*******************************************************************************************************************************************************************
    Funció que modifica una temporada.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * any: id/any de la temporada
                   * descripcio: descripció de la temporada
  *******************************************************************************************************************************************************************/
  function EditTemporada($bd_connection, $params) {
    $params['descripcio'] = NormalizeParam($params['descripcio']);

    $sql  = 'UPDATE '.db_prefix.'temporades ';
	$sql .= 'SET descripcio = \''.$params['descripcio'].'\' ';
	$sql .= 'WHERE any = '.$params['any'];
    get_sql($bd_connection, $sql, false, true);
  }
 
  /******************************************************************************************************************************************************************/
  /******************************************************************************************************************************************************************/
  /******************************************************************************************************************************************************************/

  /*******************************************************************************************************************************************************************
    Funció que retorna les temporades associats a la APP. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * any: any de la temproada a retornar els events
  *******************************************************************************************************************************************************************/
  function GetEvents($bd_connection, $params) {
    $sql =  'select ';
    $sql .= 'e.*, c.nom_curt, 0 modif, ';
    $sql .= '(select group_concat(colla) from '.db_prefix.'events_colles ec inner join '.db_prefix.'colles c on c.id = ec.id_colla where id_event = e.id group by id_event) colles ';
    $sql .= 'from '.db_prefix.'events e ';
    $sql .= '  inner join '.db_prefix.'calendaris c on c.id = e.id_cal ';
    $sql .= 'where any = '.$params['any'].' ';
    $sql .= 'order by datai, horai';

    $res = get_sql($bd_connection, $sql, false, true);
  }
 
  /*******************************************************************************************************************************************************************
    Funció que s'encarregarà de gestionar els events (altes, baixes, modificacions). 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * id: identificador de l'event (camp clau)
                   * id_cal: identificador del calendari
                   * id_google: identificador del event de Google associat
                   * any: any de la temproada
                   * descrip: descripció de l'event
                   * datai: data d'inici
                   * dataf: data de fi
                   * horai: hora d'inici
                   * horaf: hora de fi
                   * lloc: lloc on es celebrarà l'esdeveniment
				   * modif: acció a realitzar amb el registre (1 = alta, 2 = modificació, 3 = baixa)
  *******************************************************************************************************************************************************************/
  function UpdateEvents($bd_connection, $params) { 
    $params['descrip'] = NormalizeParam($params['descrip']);
    $params['lloc'] = NormalizeParam($params['lloc']);
    switch ($params['modif']) {
      case '1': // insert
                $sql = 'insert into '.db_prefix.'events (id_cal, id_google, any, descrip, datai, dataf, horai, horaf, lloc) ';
				$sql .= 'values ('.$params['id_cal'].', \''.$params['id_google'].'\', '.$params['any'].', \''.$params['descrip'].'\', \''.$params['datai'].'\', \''.$params['dataf'].'\', \''.$params['horai'].'\', \''.$params['horaf'].'\', \''.$params['lloc'].'\')';
                get_sql($bd_connection, $sql, false, true);
                break;
      case '2': // update
                $sql = 'update '.db_prefix.'events set ';
                $sql .= '  id_cal = '.$params['id_cal'].',';
                $sql .= '  id_google = \''.$params['id_google'].'\',';
                $sql .= '  any = '.$params['any'].',';
                $sql .= '  descrip = \''.$params['descrip'].'\',';
                $sql .= '  datai = \''.$params['datai'].'\',';
                $sql .= '  dataf = \''.$params['dataf'].'\',';
                $sql .= '  horai = \''.$params['horai'].'\',';
                $sql .= '  horaf = \''.$params['horaf'].'\',';
                $sql .= '  lloc  = \''.$params['lloc'].'\' ';
                $sql .= 'where id = '.$params['id'];
                get_sql($bd_connection, $sql, false, true);
                break;
      case '3': // delete
                if ($params['id'] != '') {
                  $sql = 'delete from '.db_prefix.'events where id = '.$params['id']; 
                }
                else {
                  $sql = 'delete from '.db_prefix.'events where id_google = \''.$params['id_google'].'\' and datai = \''.$params['datai'].'\'';
                }
                get_sql($bd_connection, $sql, false, true);
                break;
	}
  }
 
  /******************************************************************************************************************************************************************/
  /******************************************************************************************************************************************************************/
  /******************************************************************************************************************************************************************/

  /*******************************************************************************************************************************************************************
    Funció que retorna les colles definides en l'APP. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
  *******************************************************************************************************************************************************************/
  function GetColles($bd_connection, $params) {
    $sql = 'select * from '.db_prefix.'colles order by colla';
    $res = get_sql($bd_connection, $sql, false, true);
  }
 
?>