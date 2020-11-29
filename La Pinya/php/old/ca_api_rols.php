<?php
  /*******************************************************************************************************************************************************************
    Funció que retorna els rols definits en l'APP. 
    Paràmetres:
      - $bd_connection => objecte de connexió a la base de dades
  *******************************************************************************************************************************************************************/
  function GetRols($bd_connection) {
    $sql =  'select * ';
    $sql .= 'from '.db_prefix.'rols ';
    $sql .= 'order by descrip ';
    $res = get_sql($bd_connection, $sql, false, true);
  }
  
?>