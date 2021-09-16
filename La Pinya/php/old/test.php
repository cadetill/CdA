<?php
  define('PS_SHOP_PATH', 'http://perfumeriavip17.t4.webimpacto.net/');		// Root path of your PrestaShop store
  define('PS_WS_AUTH_KEY', 'your_api_key');	// Auth key (Get it in your Back Office)

  $url = PS_SHOP_PATH."api?ws_key=".PS_WS_AUTH_KEY;
  $xml = simplexml_load_file($url);
  echo $xml->getName().'<br>';
  print_r($xml);
?>
