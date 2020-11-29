<?php
  /*******************************************************************************************************************************************************************
    Funció que retorna els socis. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
  *******************************************************************************************************************************************************************/
  function GetSocis($bd_connection) {
    $sql =  'select s.*, ';
    $sql .= '  (select group_concat(rs.id_rol, \';\', r.descrip) ';
    $sql .= '   from '.db_prefix.'rols_socis rs ';
    $sql .= '     inner join '.db_prefix.'rols r on r.id = rs.id_rol ';
    $sql .= '   where rs.id_soci = s.id) rols ';
    $sql .= 'from '.db_prefix.'socis s ';
    $sql .= 'order by nom ';
    $res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que esborra un soci i els seus rols.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * id: identificador del calendari a esborrar
  *******************************************************************************************************************************************************************/
  function DelSoci($bd_connection, $params) {
    $sql = 'delete from '.db_prefix.'socis where id = \''.$params['id'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);

    $sql = 'delete from '.db_prefix.'rols_socis where id_soci = \''.$params['id'].'\'';
    $res1 = get_sql($bd_connection, $sql, false, false);
    
    return_affectedRows_json($bd_connection, 'ok', true, $res);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que afegeix un soci.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * nom: nom complert
                   * malnom: malnom del soci
                   * data_naix: data de naixement del soci en format yyyy-mm-dd
                   * data_alta: data d'alta del soci en format yyyy-mm-dd
                   * data_baixa: data de baixa del soci en format yyyy-mm-dd
                   * mail: correu electrònic del soci
                   * mobil: telèfon mòbil del soci
                   * is_casteller: 1 és casteller, 0 no ho és
                   * is_graller: 1 és graller, 0 no ho és
                   * sexe: H home, D Dona
                   * alt_esp: alçada a espatlles
                   * alt_ma: alçada a la ma
                   * adresa: adreça del soci
                   * poblacio: població del soci
                   * cp: codi postal del soci
                   * pais: país del soci
				   * rols: rols del soci en format X;Y;Z on X, Y i Z son els ID dels rols
  *******************************************************************************************************************************************************************/
  function AddSoci($bd_connection, $params) {
    $params['nom'] = NormalizeParam($params['nom']);
    $params['malnom'] = NormalizeParam($params['malnom']);
    if (!array_key_exists('mail', $params)) $params['mail'] = '';
    if (!array_key_exists('mobil', $params)) $params['mobil'] = '';
    if (array_key_exists('is_casteller', $params) && ($params['is_casteller'] != '')) {
      $is_casteller = $params['is_casteller'];
    } else {
      $is_casteller = 0;	
    }
    if (array_key_exists('is_graller', $params) && ($params['is_graller'] != '')) {
      $is_graller = $params['is_graller'];
    } else {
      $is_graller = 0;	
    }
    if (array_key_exists('alt_esp', $params) && ($params['alt_esp'] != '')) {
      $alt_esp = $params['alt_esp'];
    } else {
      $alt_esp = 0;	
    }
    if (array_key_exists('alt_ma', $params) && ($params['alt_ma'] != '')) {
      $alt_ma = $params['alt_ma'];
    } else {
      $alt_ma = 0;	
    }
    if (!array_key_exists('sexe', $params)) $params['sexe'] = '';
    if (!array_key_exists('adresa', $params)) $params['adresa'] = '';
    if (!array_key_exists('poblacio', $params)) $params['poblacio'] = '';
    if (!array_key_exists('cp', $params)) $params['cp'] = '';
    if (!array_key_exists('pais', $params)) $params['pais'] = '';
    if (!array_key_exists('rols', $params)) $params['rols'] = '';
    $params['adresa'] = NormalizeParam($params['adresa']);
    $params['poblacio'] = NormalizeParam($params['poblacio']);
    $sql = 'select * from '.db_prefix.'socis where nom = \''.$params['nom'].'\' and mail = \''.$params['mail'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);
    
    $rows = $bd_connection->numRows($res);
    
    $lastId = '';
    if ($rows != 0) {
      //get_sql($bd_connection, $sql, false, true);
      return_affectedRows_json($bd_connection, 'X', true, $res);
    } 
    else {
      // afegim registre
      $sql = 'insert into '.db_prefix.'socis ';
      $sql .= '(nom, malnom, data_naix, data_alta, data_baixa, mail, mobil, is_casteller, is_graller, sexe, alt_esp, alt_ma, adresa, poblacio, cp, pais) ';
      $sql .= 'values ';
      $sql .= '(\''.$params['nom'].'\', \''.$params['malnom'].'\', \''.$params['data_naix'].'\', \''.$params['data_alta'].'\', \''.$params['data_baixa'].'\', \''.$params['mail'].'\', \''.$params['mobil'].'\', '.$is_casteller.', '.$is_graller.', \''.$params['sexe'].'\', '.$alt_esp.', '.$alt_ma.', \''.$params['adresa'].'\', \''.$params['poblacio'].'\', \''.$params['cp'].'\', \''.$params['pais'].'\')';
    
      get_sql($bd_connection, $sql, false, true, $lastId);
    }
    
    // gestió dels rols. Esborrem i afegim 
    if ($lastId == '') exit;
    
    $sql = 'delete from '.db_prefix.'rols_socis where id_soci = '.$lastId;
    get_sql($bd_connection, $sql, false, false);
    if ($params['rols'] == '') exit();
    $rols = explode(';',$params['rols']); 
    foreach($rols as $key) {
      $sql = 'insert into '.db_prefix.'rols_socis (id_soci, id_rol) values ('.$lastId.', '.$key.')';
	  get_sql($bd_connection, $sql, false, false);
    }
  }
  
  /*******************************************************************************************************************************************************************
    Funció que modifica un soci.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * id: id del soci
                   * nom: nom complert
                   * malnom: malnom del soci
                   * data_naix: data de naixement del soci en format yyyy-mm-dd
                   * data_alta: data d'alta del soci en format yyyy-mm-dd
                   * data_baixa: data de baixa del soci en format yyyy-mm-dd
                   * mail: correu electrònic del soci
                   * mobil: telèfon mòbil del soci
                   * is_casteller: 1 és casteller, 0 no ho és
                   * is_graller: 1 és graller, 0 no ho és
                   * sexe: H home, D Dona
                   * alt_esp: alçada a espatlles
                   * alt_ma: alçada a la ma
                   * adresa: adreça del soci
                   * poblacio: població del soci
                   * cp: codi postal del soci
                   * pais: país del soci
				   * rols: rols del soci en format X;Y;Z on X, Y i Z son els ID dels rols
  *******************************************************************************************************************************************************************/
  function EditSoci($bd_connection, $params) {
    $params['nom'] = NormalizeParam($params['nom']);
    $params['malnom'] = NormalizeParam($params['malnom']);
    if (!array_key_exists('mail', $params)) $params['mail'] = '';
    if (!array_key_exists('mobil', $params)) $params['mobil'] = '';
    if (array_key_exists('is_casteller', $params) && ($params['is_casteller'] != '')) {
      $is_casteller = $params['is_casteller'];
    } else {
      $is_casteller = 0;	
    }
    if (array_key_exists('is_graller', $params) && ($params['is_graller'] != '')) {
      $is_graller = $params['is_graller'];
    } else {
      $is_graller = 0;	
    }
    if (array_key_exists('alt_esp', $params) && ($params['alt_esp'] != '')) {
      $alt_esp = $params['alt_esp'];
    } else {
      $alt_esp = 0;	
    }
    if (array_key_exists('alt_ma', $params) && ($params['alt_ma'] != '')) {
      $alt_ma = $params['alt_ma'];
    } else {
      $alt_ma = 0;	
    }
    if (!array_key_exists('sexe', $params)) $params['sexe'] = '';
    if (!array_key_exists('adresa', $params)) $params['adresa'] = '';
    if (!array_key_exists('poblacio', $params)) $params['poblacio'] = '';
    if (!array_key_exists('cp', $params)) $params['cp'] = '';
    if (!array_key_exists('pais', $params)) $params['pais'] = '';
    if (!array_key_exists('rols', $params)) $params['rols'] = '';
    $params['adresa'] = NormalizeParam($params['adresa']);
    $params['poblacio'] = NormalizeParam($params['poblacio']);

    $sql  = 'UPDATE '.db_prefix.'socis ';
	$sql .= 'SET nom = \''.$params['nom'].'\', malnom = \''.$params['malnom'].'\', data_naix = \''.$params['data_naix'].'\', data_alta = \''.$params['data_alta'].'\', data_baixa = \''.$params['data_baixa'].'\', mail = \''.$params['mail'].'\', mobil = \''.$params['mobil'].'\', is_casteller = \''.$is_casteller.'\', is_graller = \''.$is_graller.'\', sexe = \''.$params['sexe'].'\', alt_esp = '.$alt_esp.', alt_ma = '.$alt_ma.', adresa = \''.$params['adresa'].'\', poblacio = \''.$params['poblacio'].'\', cp = \''.$params['cp'].'\', pais = \''.$params['pais'].'\' ';
	$sql .= 'WHERE id = '.$params['id'];
    get_sql($bd_connection, $sql, false, true);
    
    // gestió dels rols. Esborrem i afegim 
    $sql = 'delete from '.db_prefix.'rols_socis where id_soci = '.$params['id'];
    get_sql($bd_connection, $sql, false, false);
    if ($params['rols'] == '') exit();
    $rols = explode(';',$params['rols']); 
    foreach($rols as $key) {
      $sql = 'insert into '.db_prefix.'rols_socis (id_soci, id_rol) values ('.$params['id'].', '.$key.')';
	  get_sql($bd_connection, $sql, false, false);
    }
  }
  
  
  
 /*******************************************************************************************************************************************************************
    Funció que afegeix un dispositiu.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * uuid: identificador únic per al dispositiu (GUID o data-hora en format yyyymmddhhnnss)
                   * so: sistema operatiu del dispositiu
  *******************************************************************************************************************************************************************/
  function AddDispositiu($bd_connection, $params) {
    $params['uuid'] = NormalizeParam($params['uuid']);
    $params['so'] = NormalizeParam($params['so']);

    $sql = 'select * from '.db_prefix.'dispositius where uuid = \''.$params['uuid'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);
    
    $rows = $bd_connection->numRows($res);

    if ($rows != 0) {
      get_sql($bd_connection, $sql, false, true);
    } 
    else {
      // afegim registre
      $sql = 'insert into '.db_prefix.'dispositius (uuid, so, alta) values (\''.$params['uuid'].'\', \''.$params['so'].'\', \''.date("Y-m-d").'\')';
      get_sql($bd_connection, $sql, false, true);
    }
  }
 
  /*******************************************************************************************************************************************************************
    Funció que genera el Activation Code agafant aleatòriament 8 dígits alfanumèrics. Comprova que no existeixi a la taula ca_disp_socis
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $length => longitud de la cadena a retornar. Per defecte 8
    Retorna: string amb el Activation Code
  *******************************************************************************************************************************************************************/
  function generateRandomString($bd_connection, $length = 8) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $charactersLength = strlen($characters);
    
    do {
      $exit = false;
      $randomString = '';
      for ($i = 0; $i < $length; $i++) {
         $randomString .= $characters[rand(0, $charactersLength - 1)];
      }
      $sql = 'select * from '.db_prefix.'disp_socis where actcode = \''.$randomString.'\'';
      $res = get_sql($bd_connection, $sql, false, false);
      $exit = $bd_connection->numRows($res) == 0;
    } while (!$exit);
    
    return $randomString;
  }
  
  /*******************************************************************************************************************************************************************
    Funció que retorna les temporades associats a la APP. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * id: identificador del dispositiu associat a l'usuari
                   * mail: correu electrònic del soci
                   * mobil: mòbil del soci
                   * data_naix: data de naixement del soci
  *******************************************************************************************************************************************************************/
  function GetUserCode($bd_connection, $params) {
    // mirem si existeix el soci amb les dades introduïdes en la pantalla
    $sql = 'select * from '.db_prefix.'socis where data_naix = \''.$params['data_naix'].'\' and mobil = \''.$params['mobil'].'\' and mail = \''.$params['mail'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);
    $rows = $bd_connection->numRows($res);

    if ($rows != 0) { // existeix, tenim 2 opcions, que ja s'hagi identificat (reenviar codi), que no ho hagi fet (crear codi i enviar)
      // agafem info registre
      $row = $res->fetch_assoc();
      
      // mirem si existeix relació dispositiu-soci
      $sql = 'select * from '.db_prefix.'disp_socis where id_disp = '.$params['id'].' and id_soci = '.$row['id'];
      $res = get_sql($bd_connection, $sql, false, false);
      $rows = $bd_connection->numRows($res);
      
      if ($rows != 0) { // ja estava d'alta, reenviem Activation Code
        $row1 = $res->fetch_assoc();
        $code = $row1['actcode'];
      }
      else { // no estava donat d'alta, creem nou Activation Code i l'afegim
        $code = generateRandomString($bd_connection, 8);
        $sql = 'insert into '.db_prefix.'disp_socis (id_disp, id_soci, actcode) values ('.$params['id'].', '.$row['id'].', \''.$code.'\')';
        get_sql($bd_connection, $sql, false, false);
      }
      
      // enviem correu 
      $missatge = "Registre del programa CdAPP dels Castellers d'Andorra\n\n"; 
      $missatge .= "Aquestes són les teves dades de registre:\n"; 
      $missatge .= "Correu electronic: ".$params['mail']."\n"; 
      $missatge .= "Telèfon de contacte: ".$params['mobil']."\n"; 
      $missatge .= "Data de Naixement: ".$params['data_naix']."\n\n"; 
      $missatge .= "Per poder entrar a la CdAPP hauràs d'introduir el següent codi:\n"; 
      $missatge .= "Codi: ".$code."\n\n\n"; 
      $missatge .= "Moltes gràcies per registrar-te de part de tot l'equip tècnic i directiu, i esperem que gaudeixis de l'aplicació.\n\n"; 
      $missatge .= "Ens veiem a l'assaig!"; 
      
      $assumpte = "Activació del compte a CdAPP"; 
        
      $headers = "From : ".webmaster_email;
        
      if (mail($params['mail'], $assumpte, $missatge, $headers) == false) {
        return_emty_json($bd_connection, '', true);
      } 
      else {
        $sql = 'select actcode from '.db_prefix.'disp_socis where id_disp = '.$params['id'].' and id_soci = '.$row['id'];  
        get_sql($bd_connection, $sql, false, true);
      }
    } 
    else { // no existeix, retornar error
      return_emty_json($bd_connection, '', true);
    }
  }
  
  /*******************************************************************************************************************************************************************
    Funció que retorna els rols del soci identificat en la APP. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * actcode: codi d'activació relacionat amb l'usuari
  *******************************************************************************************************************************************************************/
  function GetRolsSoci($bd_connection, $params) {
    $sql  = 'select d.id_soci, r.id_rol ';
    $sql .= 'from '.db_prefix.'disp_socis d ';
    $sql .= '  inner join '.db_prefix.'rols_socis r on r.id_soci = d.id_soci ';
    $sql .= 'where d.actcode = \''.$params['actcode'].'\'';
	$res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que retorna informació sobre un soci. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * actcode: codi d'activació relacionat amb l'usuari
  *******************************************************************************************************************************************************************/
  function GetSociInfo($bd_connection, $params) {
    $sql  = 'select s.*, r.id_rol ';
    $sql .= 'from '.db_prefix.'disp_socis d ';
    $sql .= '  inner join '.db_prefix.'socis s on s.id = d.id_soci ';
    $sql .= '  inner join '.db_prefix.'rols_socis r on r.id_soci = d.id_soci ';
    $sql .= 'where d.actcode = \''.$params['actcode'].'\'';
	$res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que retorna el nom d'un o més socis. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * actcode: codis d'activació dels socis (separats per ,)
  *******************************************************************************************************************************************************************/
  function GetSociName($bd_connection, $params) {
    $params['actcode'] = explode(',', $params['actcode']);
    $where = '';
    foreach ($params['actcode'] as $valor) {
      if ($where != '') $where .= ',';
      $where .= '\''.$valor.'\'';
    }
    
    $sql  = 'select d.actcode, s.nom ';
    $sql .= 'from '.db_prefix.'disp_socis d ';
    $sql .= '  inner join '.db_prefix.'socis s on s.id = d.id_soci ';
    $sql .= 'where d.actcode in ('.$where.')';
	$res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que desvincula un soci d'un dispositiu.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * actcode: codi d'activació del soci a desvincular
                   * id: identificador del dispositiu desvincular de l'usuari
  *******************************************************************************************************************************************************************/
  function DesvinculaSoci($bd_connection, $params) {
    $sql = 'delete from '.db_prefix.'disp_socis where id_disp = '.$params['id'].' and actcode = \''.$params['actcode'].'\'';
    get_sql($bd_connection, $sql, false, false);
    
    return_emty_json($bd_connection, 'ok', true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que retorna els events associats al casteller.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * actcode: codi d'activació del soci 
                   * any: any dels events 
  *******************************************************************************************************************************************************************/
  function GetAssistencia($bd_connection, $params) {
    $sql  = 'select a.* ';
    $sql .= 'from '.db_prefix.'disp_socis d ';
    $sql .= '  inner join '.db_prefix.'assistencia a on a.id_soci = d.id_soci ';
    $sql .= '  inner join '.db_prefix.'events e on e.id = a.id_event ';
    $sql .= 'where d.actcode = \''.$params['actcode'].'\' ';
    $sql .= '  and e.any = '.$params['any'];
  
    $res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que retorna informació sobre un event d'un soci.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * actcode: codi d'activació del soci 
                   * idevent: identificador de l'esvent 
  *******************************************************************************************************************************************************************/
  function GetInfoEvent($bd_connection, $params) {
    $sql  = 'select a.* ';
    $sql .= 'from '.db_prefix.'disp_socis d ';
    $sql .= '  left outer join '.db_prefix.'assistencia a on a.id_soci = d.id_soci and a.id_event = '.$params['idevent'].' ';
    $sql .= 'where d.actcode = \''.$params['actcode'].'\' ';
    
    $res = get_sql($bd_connection, $sql, false, true);
  }
  
  /*******************************************************************************************************************************************************************
    Funció que actualitza un event per a un soci.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * idsoci: identificador del soci 
                   * idevent: identificador de l'esvent 
                   * bus: indica si anirà (1) o no (0) en bus 
                   * assist: indica si anirà (1), no anirà (0) o no ho sap (2)
  *******************************************************************************************************************************************************************/
  function UpdateInfoEventSoci($bd_connection, $params) {
    $sql  = 'INSERT INTO '.db_prefix.'assistencia (id_soci, id_event, bus, assist) ';
	$sql .= 'VALUES ('.$params['idsoci'].', '.$params['idevent'].', '.$params['bus'].', '.$params['assist'].') ';
	$sql .= 'ON DUPLICATE KEY UPDATE bus='.$params['bus'].', assist='.$params['assist'];
    get_sql($bd_connection, $sql, false, false);
    
    return_emty_json($bd_connection, 'ok', true);
  }
?>