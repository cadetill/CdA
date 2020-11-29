<?php

  // require section
  require_once 'ca_base.php';
  require_once 'ca_config.php';
  //require_once 'ca_members.php';
  require_once 'ca_calendars.php';

  // config object  
  $conf = new ca_config();
  
  // function needed
  if (filter_input(INPUT_SERVER, 'REQUEST_METHOD') === 'POST') {
    $request = json_decode(file_get_contents("php://input"), true);
    $func = $request['func'];
  }
  else {
    $func = filter_input(INPUT_GET, 'func');
  }


  // check if it's a correct function name
  if (!$conf->isCorrectFunc($func)) {
    return $conf->errBadRequest;
  }
  
  // function switch    
  switch ($func) {
    case 'getCalendars': 
    case 'delCalendar': 
    case 'addCalendar': 
    case 'editCalendar': 
      $cal = new ca_calendars($conf, $func);
      $response = $cal->execute();
      break;
    default:
      return $this->conf->errFuncNotFound;
  }

  // if empty string, return an error
  if ($response == '') {
    echo $conf->errNoResult;
    exit;
  }
  
  // if array, convert to a Json
  if (is_array($response)) {
    $response = json_encode($response, true);
  }

  // returns Json
  echo $response;