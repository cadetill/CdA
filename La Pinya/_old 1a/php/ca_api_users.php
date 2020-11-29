<?php
  /*******************************************************************************************************************************************************************
    Funció que crea un nou usuari de La Pinya. 
    L'usuari no ha d'existir ni per usuari ni per correu electrònic.
    La contrasenya es guardarà cifrada.
    En cas de crear-se l'usuari, se li enviarà un correu d'activació amb un enllaç per activar el compte. Si no s'activa, no podrà utilitzar l'App.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * user: usuari que es vol donar d'alta
                   * pass: mot clau de l'usuari
                   * mail: correu electrònic de l'usuari
  *******************************************************************************************************************************************************************/
  function new_user($bd_connection, $params) {
    $sql = 'select * from '.db_prefix.'users where user = \''.$params['user'].'\' or mail = \''.$params['mail'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);
    
    $rows = $bd_connection->numRows($res);
    
    if ($rows != 0) {
      return_emty_json($bd_connection, '', true);
    } 
    else {
      // afegim usuari a la base de dades 
      $codi = uniqid(); // Genera un id únic per a identificar el compte de correu.
      $passHash = password_hash($params['pass'], PASSWORD_BCRYPT); // encriptem password
      $sql = 'insert into '.db_prefix.'users (user, pass, mail, active, codact) values (\''.$params['user'].'\', \''.$passHash.'\', \''.$params['mail'].'\', 0, \''.$codi.'\')';
      get_sql($bd_connection, $sql, false, true);
      
      // afegim rol de "convidat" a l'usuari
      $sql = 'insert into '.db_prefix.'users_rols (id_user, id_rol) values ('.$bd_connection->getLastId().', 6)';
      $res = get_sql($bd_connection, $sql, false, false);
      
      // enviem mail d'activació
	  $params[$withinfo] = false;
      SendActivationMail($bd_connection, $params);
    }
  }
  
  /*******************************************************************************************************************************************************************
    Funció que valida si un usuari que s'intenta loggejar és correcte. En cas de ser correcte, retorna els rols de l'usuari.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * user: usuari que es vol donar d'alta
                   * pass: mot clau de l'usuari
  *******************************************************************************************************************************************************************/
  function IsValidUser($bd_connection, $params) {
    $sql = 'select * from '.db_prefix.'users where user = \''.$params['user'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);
    
    $rows = $bd_connection->numRows($res);
    if ($rows == 0) {
      $sql = 'select status, 0 id, \'\' user, \'\' id_soci, \'\' active, \'\' id_rol
              from '.db_prefix.'status 
              where status = \'\'';
      get_sql($bd_connection, $sql, false, true);
      //return_emty_json($bd_connection, '', true);
      return;
    } 
    
    $row = $res->fetch_assoc();
    if (password_verify($params['pass'], $row['pass'])) {
      $sql = 'select (select status from '.db_prefix.'status where status <> \'\') status, u.id, u.user, u.id_soci, u.active, ur.id_rol
              from '.db_prefix.'users u
                left outer join '.db_prefix.'users_rols ur on ur.id_user = u.id 
              where u.id = '.$row['id'];
      get_sql($bd_connection, $sql, false, true);
    }    
    else {
      return_emty_json($bd_connection, '', true);
      return;
    }
  }
  
  /*******************************************************************************************************************************************************************
    Funció que envia el correu d'activació a l'usuari especificat. Comprova que realment estigui desactivat.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
      - $params => paràmetres rebuts per url
                   * user: usuari que es vol donar d'alta
                   * withinfo: especifica si s'ha de retornar resposta (JSON) de l'estat de l'enviament del mail
  *******************************************************************************************************************************************************************/
  function SendActivationMail($bd_connection, $params, $withinfo) {
    $sql = 'select * from '.db_prefix.'users where user = \''.$params['user'].'\'';
    $res = get_sql($bd_connection, $sql, false, false);
    
    $rows = $bd_connection->numRows($res);
    
    if ($rows == 0) { // no existeix l'usuari => error i marxem
      return_emty_json($bd_connection, '', true);
      return;
    } 
    
    $row = $res->fetch_assoc();
    if ($row['active'] == 1) {  // si ja està activat, error i marxem
      return_emty_json($bd_connection, '', true);
      return;
    }
      
    // enviem mail d'activació
    $missatge = "Registre del programa La Pinya dels Castellers d'Andorra\n\n"; 
    $missatge .= "Aquestes són les teves dades de registre:\n"; 
    $missatge .= "Usuari: ".$params['user']."\n\n"; 
    $missatge .= "Has d'activar el compte polsant el següent enllaç: http://www.castellersandorra.com/ca/activation.php?id=".$row['codact']."\n\n"; 
    $missatge .= "Moltes gràcies per registrar-te de part de tot l'equip tècnic i esperem que gaudeixis de l'aplicació.\n\n"; 
    $missatge .= "Ens veiem a l'assaig!"; 
      
    $assumpte = "Activació del compte a La Pinya"; 
      
    $headers = "From : ".webmaster_email;
      
    if (mail($row['mail'], $assumpte, $missatge, $headers) == false) {
      echo 'mierdaaaaaaa';
    } 
    else {
      if ($params['withinfo'] == true) {
        return_emty_json($bd_connection, 'ok', false);
      }
    }
  }
  
  /*******************************************************************************************************************************************************************
    Funció que retorna tots els usuaris donats d'alta en la App.
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
  *******************************************************************************************************************************************************************/
  function GetUsersList($bd_connection) {
    $sql = 'select id, user, mail, id_soci, active from '.db_prefix.'users';
    $res = get_sql($bd_connection, $sql, false, true);
  }  
?>