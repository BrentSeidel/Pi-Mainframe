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
function on_load()
{
  getCPUInfo();
  reload_timer = window.setInterval(update_values, 1000);
}
//
//  Update values.  This is called periodically to update values that may
//  change on the host side.
//
function update_values()
{
  updateRegisters();
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
      displayCPUInfo(this);
    }
  };
  xhttp.open("GET", "/xml/cpu-info", true);
  xhttp.send();
}
//
// Displays the simulated CPU information.
//
function displayCPUInfo(xml)
{
  var xmlDoc = xml.responseXML;
  var table;
  var row;
  var cell;
  var index;

  sim_name = xmlDoc.getElementsByTagName("cpu-name")[0].childNodes[0].nodeValue;
  memory_size = Number(xmlDoc.getElementsByTagName("cpu-mem")[0].childNodes[0].nodeValue);
  num_registers = Number(xmlDoc.getElementsByTagName("cpu-reg")[0].childNodes[0].nodeValue);
  document.getElementById("Sim Name").textContent = sim_name;
  document.getElementById("Num Reg").textContent = num_registers;
  document.getElementById("Mem Size").textContent = memory_size;
  table = document.getElementById("Registers");
  for (index = 0; index < num_registers; index++)
  {
      row = table.insertRow(-1);
      cell = row.insertCell(0);
      cell.innerHTML = index;
      cell = row.insertCell(1);
      cell.id = "Reg-Name-" + index;
      cell = row.insertCell(2);
      cell.id = "Reg-Value-" + index;
      cell.style.textAlign = "right";
      getRegister(index);
  }
}
//======================================
//
//  Update register values.  This is done periodically.
//
function updateRegisters()
{
  var index;

  for (index = 0; index < num_registers; index++)
  {
      getRegister(index);
  }
}
//
// Requests register value from the server using AJAX.
//
function getRegister(val)
{
  var xhttp = new XMLHttpRequest();

  xhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      displayRegValue(this);
    }
  };
  xhttp.open("GET", "/xml/cpu-reg?register=" + val, true);
  xhttp.send();
}
//
// Displays the value requested in the previous function in the "sim-type" element.
//
function displayRegValue(xml)
{
  var xmlDoc = xml.responseXML;
  var num = Number(xmlDoc.getElementsByTagName("reg-num")[0].childNodes[0].nodeValue);
  var name = xmlDoc.getElementsByTagName("reg-name")[0].childNodes[0].nodeValue
//  var value = Number(xmlDoc.getElementsByTagName("reg-value")[0].childNodes[0].nodeValue)
  var value = xmlDoc.getElementsByTagName("reg-value")[0].childNodes[0].nodeValue

  document.getElementById("Reg-Name-" + num).textContent = name;
//  document.getElementById("Reg-Value-" + num).textContent = value.toString(16);
  document.getElementById("Reg-Value-" + num).textContent = value;
}
