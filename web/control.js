"use strict";
//
// Call this routine when the page is finished loading.
//
function page_loaded()
{
  getAutoMan();
  getSimType();
}
//======================================
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
// Displays the value requested in the previous function in the "auto-man" element.
//
function displayAutoMan(xml)
{
  var xmlDoc = xml.responseXML;
  var auto_man = xmlDoc.getElementsByTagName("auto-man")[0].childNodes[0].nodeValue;

//  console.log("auto-man value is " + auto_man);
  if (auto_man == "TRUE")
  {
    document.getElementById("auto_man").checked = true;
  }
  else
  {
    document.getElementById("auto_man").checked = false;
  }
}
//======================================
//
//  Get the value from the auto-man checkbox and send it to the server.
//
function setSimType(sim)
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      displaySimType(this);
    }
  };
  xhttp.open("GET", "/xml/sim-type?sim-type=" + sim, true);
  xhttp.send();
}
//
// Requests sim-type value from the server using AJAX.
//
function getSimType()
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      displaySimType(this);
    }
  };
  xhttp.open("GET", "/xml/sim-type", true);
  xhttp.send();
}
//
// Displays the value requested in the previous function in the "sim-type" element.
//
function displaySimType(xml)
{
  var xmlDoc = xml.responseXML;
  var sim_type = Number(xmlDoc.getElementsByTagName("pattern")[0].childNodes[0].nodeValue);
  var ids = ["sim-copy", "sim-count", "sim-scan", "sim-bounce", "sim-fib"][sim_type];

//  console.log("Sim-type array is: " + ids);
//  console.log("sim-type value is " + sim_type + " (" + ids + ")");
  document.getElementById(ids).checked = true;
}
