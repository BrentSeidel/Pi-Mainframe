"use strict";
//
//  Global variables
//
var reload_timer;
var sim_name;
var memory_size;
var num_registers
//
//  Call this routine when the page is finished loading.  It sets a
//  periodic task to update values every second.
//
function page_loaded()
{
  getCPUInfo();
  getAutoMan();
  getSimType();
  getPanelReg();
}
//
//  Update values.  This is called periodically to update values that may
//  change on the host side.
//
function update_values()
{
  getAutoMan();
  getSimType();
  getPanelReg();
}
//======================================
//
// Requests simulated CPU information from the server using AJAX.
//
function getCPUInfo()
{
  var xhttp = new XMLHttpRequest();


  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      setCPUInfo(this);
    }
  };
  xhttp.open("GET", "/xml/cpu-info", true);
  xhttp.send();
}
//
// Set the simulated CPU information.
//
function setCPUInfo(xml)
{
  var xmlDoc = xml.responseXML;
  var temp;

  sim_name = xmlDoc.getElementsByTagName("cpu-name")[0].childNodes[0].nodeValue;
  memory_size = Number(xmlDoc.getElementsByTagName("cpu-mem")[0].childNodes[0].nodeValue);
  num_registers = Number(xmlDoc.getElementsByTagName("cpu-reg")[0].childNodes[0].nodeValue);
  temp = "Simulator name is " + sim_name + ".  ";
  temp += "The number of registers is " + num_registers + ".  ";
  temp += "The memory size is " + memory_size + ".";
  document.getElementById("Sim").innerHTML = temp;
//  if (sim_name == "Example simulator")
//  {
//    reload_timer = window.setInterval(update_values, 1000);
//  }
//  else
//  {
//    window.location.href = "/Information";
//  }
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
  var auto_enable = xmlDoc.getElementsByTagName("auto-enable")[0].childNodes[0].nodeValue;

//  console.log("auto-man value is " + auto_man);
  if (auto_man == "TRUE")
  {
    document.getElementById("auto_man").checked = true;
  }
  else
  {
    document.getElementById("auto_man").checked = false;
  }
  if (auto_enable == "TRUE")
  {
    document.getElementById("auto_man").disabled = false;
  }
  else
  {
    document.getElementById("auto_man").disabled = true;
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

  if (sim_type == 1)
  {
    document.getElementById("sim-count").checked = true;
    return;
  }
  if (sim_type == 2)
  {
    document.getElementById("sim-scan16").checked = true;
    return;
  }
  if (sim_type == 3)
  {
    document.getElementById("sim-bounce16").checked = true;
    return;
  }
  if (sim_type == 4)
  {
    document.getElementById("sim-fib").checked = true;
    return;
  }
  if (sim_type == 9)
  {
    document.getElementById("sim-count").checked = true;
    return;
  }
  if (sim_type == 10)
  {
    document.getElementById("sim-scan32").checked = true;
    return;
  }
  if (sim_type == 11)
  {
    document.getElementById("sim-bounce32").checked = true;
    return;
  }
  if (sim_type == 12)
  {
    document.getElementById("sim-fib").checked = true;
    return;
  }
  document.getElementById("sim-copy").checked = true;
}
//======================================
//
//  Get panel switch register values and display them in the web page
//
function getPanelReg()
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      displayPanelReg(this);
    }
  };
  xhttp.open("GET", "/xml/panel-reg", true);
  xhttp.send();
}
//
// Displays the value requested in the previous function in the "sim-type" element.
//
function displayPanelReg(xml)
{
  var xmlDoc = xml.responseXML;
  var lr_ad  = Number(xmlDoc.getElementsByTagName("lr-ad")[0].childNodes[0].nodeValue);
  var lr_ctl = Number(xmlDoc.getElementsByTagName("lr-ctl")[0].childNodes[0].nodeValue);
  var sr_ad  = Number(xmlDoc.getElementsByTagName("sr-ad")[0].childNodes[0].nodeValue);
  var sr_ctl = Number(xmlDoc.getElementsByTagName("sr-ctl")[0].childNodes[0].nodeValue);

//  console.log("lr_ad: " + lr_ad.toString(16));
  document.getElementById("lr_ad").value  = lr_ad.toString(16);
  document.getElementById("lr_ctl").value = lr_ctl.toString(16);
  document.getElementById("sr_ad").value  = sr_ad.toString(16);
  document.getElementById("sr_ctl").value = sr_ctl.toString(16);
}
