"use strict";
//
// Call this routine when the page is finished loading.
//
function page_loaded()
{
  getAutoMan();
}
//
//  Get the value from the auto-man checkbox and send it to the server.
//
function setAutoMan()
{
  var xhttp = new XMLHttpRequest();
  var auto_man = document.getElementById("auto_man").checked;

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      displayAutoMan(this);
    }
  };
  xhttp.open("GET", "/xml/auto-man?auto-man=" + auto_man, true);
  xhttp.send();
}
//
// Requests auto-man value from the server using AJAX.
//
function getAutoMan()
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      displayAutoMan(this);
    }
  };
  xhttp.open("GET", "/xml/auto-man", true);
  xhttp.send();
}
//
// Displays the value requested in the previous function in the "counter" element.
//
function displayAutoMan(xml)
{
  var xmlDoc = xml.responseXML;
  var auto_man = xmlDoc.getElementsByTagName("auto-man")[0].childNodes[0].nodeValue;

  console.log("auto-man value is " + auto_man);
  if (auto_man == "TRUE")
  {
    document.getElementById("auto_man").checked = true;
  }
  else
  {
    document.getElementById("auto_man").checked = false;
  }
}
