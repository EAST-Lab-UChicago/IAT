template = {};
sub = '';

// Escape text from the template before inserting it into HTML
function escapeHtml(str)
{
	return String(str).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
		.replace(/"/g, "&quot;").replace(/'/g, "&#39;");
}

function randomString(length) {
	var chars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    var result = '';
    for (var i = length; i > 0; --i) result += chars.charAt(Math.floor(Math.random() * (chars.length - 1)));
    return result;
}

// Loads the input file and starts introduction
function initialize()
{	
	// get active template & load data into global variable
	$.getJSON("templates/active.txt", function(input) {
		document.title = "IAT";
		$.getJSON("templates/"+input.active+"/input.txt", function(data) { 
			template = data;
			$.get("core/instruct0.html", function(data) {
				$("#instructions").html(data);
				$("#subID").val("");
			});
		});
	});
	
}

function loadInstructions(stage)
{
	switch(stage)
	{
		case 'one':
			sub = $("#subID").val();
			if(sub.length >= 24 && sub.length <= 28 && /^[a-zA-Z0-9]+$/.test(sub))
			{
				$.get("core/instruct1.html", function(data) {
					$("#instructions").html(data);
					$(".IATname").text(template.name);
					if(	template.catA.itemtype == "img" || 
						template.catB.itemtype == "img" || 
						template.cat1.itemtype == "img" || 
						template.cat2.itemtype == "img")
					{
						$("#andpics").html(" and pictures ");
					}
				});
			}
			else
			{
				alert("Please enter a valid Prolific ID");
			}
			break;
		case 'two':
			$.get("core/instruct2.html", function(data) {
				$("#instructions").html(data);
				
				$("#clabel1").text(template.cat1.label);
		        $("#clabel2").text(template.cat2.label);
		        $("#clabelA").text(template.catA.label);
		        $("#clabelB").text(template.catB.label);
		        if (template.cat1.itemtype == "txt")
		            { $("#citems1").text(template.cat1.items.join(", ")); }
		        else if (template.cat1.itemtype == "img")
		            { $("#citems1").text("Images of "+template.cat1.label); }
		        if (template.cat2.itemtype == "txt")
		            { $("#citems2").text(template.cat2.items.join(", ")); }
		        else if (template.cat2.itemtype == "img")
		            { $("#citems2").text("Images of "+template.cat2.label); }
		        if (template.catA.itemtype == "txt")
		            { $("#citemsA").text(template.catA.items.join(", ")); }
		        else if (template.catA.itemtype == "img")
		            { $("#citemsA").text("Images of "+template.catA.label); }
		        if (template.catB.itemtype == "txt")
		            { $("#citemsB").text(template.catB.items.join(", ")); }
		        else if (template.catB.itemtype == "img")
		            { $("#citemsB").text("Images of "+template.catB.label); }
			});
			break;
		case 'IAT':
			$.get("core/IAT.html", function(data) {
				$('body').html(data);
				document.onkeypress = keyHandler; 
				startIAT();
			});
			break;
	}
}

// Initialize variables, build page & data object, display instructions
function startIAT()
{
	currentState = "instruction";
	session = 0;
	roundnum = 0;
	
	// default to show results to participant
	if (!('showResult' in template))
	{
	    template.showResult = "show";
	}
	
	// make the target or association words green
	if (Math.random() < 0.5)
	{
		openA = "<font color=green>";
		closeA = "</font>";
		open1 = "";
		close1 = "";
	}
	else
	{		
		open1 = "<font color=green>";
		close1 = "</font>";
		openA = "";
		closeA = "";
	}
	buildPage();
	roundArray = initRounds();
    instructionPage();
    
}

// Adds all images to page (initially hidden) so they are pre-loaded for IAT
function buildPage()
{
	var cats = [template.catA, template.catB, template.cat1, template.cat2];
	for (var c = 0; c < cats.length; c++)
	{
		if (cats[c].itemtype == "img")
		{
			for (var i = 0; i < cats[c].items.length; i++)
			{
				var item = $('<img>', {
					'id': cats[c].datalabel + i,
					'class': 'IATitem',
					'src': 'templates/' + encodeURIComponent(template.name) + '/img/' + encodeURIComponent(cats[c].items[i])
				});
				$("#exp_instruct").after(item);
			}
		}
	}
}

// Round object
function IATround()
{
	this.starttime = 0;
	this.endtime = 0;
	this.itemtype = "none";
	this.category = "none";
	this.catIndex = 0;
	this.correct = 0;
	this.errors = 0;
}

// Create array for each session & round, with pre-randomized ordering of images
function initRounds()
{
    var roundArray = [];
    // for each session
    for (var i=0; i<7; i++)
    {
        roundArray[i] = [];
        switch (i)
        {
            case 0:
            case 4:
                stype = "target";
                numrounds = 20;
                break;
            case 1:    
                stype = "association";
                numrounds = 20;
                break;
            case 2:
            case 3:
            case 5:
            case 6:
                stype = "both";
                numrounds = 40;
                break;
            
        }
		prevIndexA = -1; prevIndex1 = -1;
        for (var j = 0; j<numrounds; j++)
        {
            var round = new IATround();
            
            if (stype == "target")
            {
                round.category = (Math.random() < 0.5 ? template.catA.datalabel : template.catB.datalabel);
            }
            else if (stype == "association")
            {
                round.category = (Math.random() < 0.5 ? template.cat1.datalabel : template.cat2.datalabel);  
            }
            else if (stype == "both")
            {
				if (j % 2 == 0) { round.category = (Math.random() < 0.5 ? template.catA.datalabel : template.catB.datalabel); }
				else { round.category = (Math.random() < 0.5 ? template.cat1.datalabel : template.cat2.datalabel); }
            }
        	// pick a category
        	if (round.category == template.catA.datalabel) 
        	{ 
				round.itemtype = template.catA.itemtype;
				if (i < 4) { round.correct = 1; }
				else { round.correct = 2; }
				
				// pick an item different from the last
				do 
					{ round.catIndex = Math.floor(Math.random()*template.catA.items.length); }
	        	while (template.catA.items.length > 1 && prevIndexA == round.catIndex)
	        	prevIndexA = round.catIndex;
        		
        	}
        	else if (round.category == template.catB.datalabel)
        	{ 
				round.itemtype = template.catB.itemtype;
				if (i < 4) { round.correct = 2; }
				else { round.correct = 1; }
				// pick an item different from the last
				do
	        	    { round.catIndex = Math.floor(Math.random()*template.catB.items.length); }
	        	while (template.catB.items.length > 1 && prevIndexA == round.catIndex)
	        	prevIndexA = round.catIndex;
        	}
        	else if (round.category == template.cat1.datalabel)
        	{ 
				round.itemtype = template.cat1.itemtype;
        		round.correct = 1;
				// pick an item different from the last
				do
	        	    { round.catIndex = Math.floor(Math.random()*template.cat1.items.length); }
	        	while (template.cat1.items.length > 1 && prevIndex1 == round.catIndex)
	        	prevIndex1 = round.catIndex;
        	}
        	else if (round.category == template.cat2.datalabel)
        	{ 
				round.itemtype = template.cat2.itemtype;
        		round.correct = 2;
				// pick an item different from the last
				do
	        	    { round.catIndex = Math.floor(Math.random()*template.cat2.items.length); }
	        	while (template.cat2.items.length > 1 && prevIndex1 == round.catIndex)
	        	prevIndex1 = round.catIndex;
        	}	
        	
        	roundArray[i].push(round);
        }
    }
    
    return roundArray;
}

// insert instruction text based on stage in IAT
function instructionPage()
{	
	switch (session)
    {
		case 0:
			$('#left_cat').ready(function() { $('#left_cat').html(openA+escapeHtml(template.catA.label)+closeA); });
			$('#right_cat').ready(function() { $('#right_cat').html(openA+escapeHtml(template.catB.label)+closeA); });
			break;
        case 1:    
			$("#left_cat").html(open1+escapeHtml(template.cat1.label)+close1);
			$("#right_cat").html(open1+escapeHtml(template.cat2.label)+close1);
            break;
        case 2:
        case 3:
			$("#left_cat").html(openA+escapeHtml(template.catA.label)+closeA+'<br>or<br>'+open1+escapeHtml(template.cat1.label)+close1);
			$("#right_cat").html(openA+escapeHtml(template.catB.label)+closeA+'<br>or<br>'+open1+escapeHtml(template.cat2.label)+close1);
            break;
        case 4:
			$("#left_cat").html(openA+escapeHtml(template.catB.label)+closeA);
			$("#right_cat").html(openA+escapeHtml(template.catA.label)+closeA);
			break;
        case 5:
        case 6:
			$("#left_cat").html(openA+escapeHtml(template.catB.label)+closeA+'<br>or<br>'+open1+escapeHtml(template.cat1.label)+close1);
			$("#right_cat").html(openA+escapeHtml(template.catA.label)+closeA+'<br>or<br>'+open1+escapeHtml(template.cat2.label)+close1);
            break;
    }
	if (session == 7)
	{
		$("#left_cat").html("");
		$("#right_cat").html("");
		$("#exp_instruct").html("<img src='core/spinner.gif'>");
		WriteFile();
		if(template.showResult == "show")
		{
		    calculateIAT();
		}
		else
		{
		    resulttext = "<div style='text-align:center;padding:20px'>Thanks for completing this portion of the study! Enter this completion code `42734732` in Qualtrics to continue the rest of the survey.</div>";
		    $("#picture_frame").html(resulttext);
		}
	}
	else
	{
		$.get("core/gInstruct"+(session+1)+".html", function(data) { $('#exp_instruct').html(data); });
	}
}

// Calculates estimate of effect size to present results to participant
function calculateIAT()
{
    // calculate mean log(RT) for first key trial
	compatible = 0;
	for (i=1; i<roundArray[3].length; i++)
	{
		score = roundArray[3][i].endtime - roundArray[3][i].starttime;
		if (score < 300) { score = 300; }
		if (score > 3000) { score = 3000; }
		compatible += Math.log(score);
	}
	compatible /= (roundArray[3].length - 1);
	
	// calculate mean log(RT) for second key trial
	incompatible = 0;
	for (i=1; i<roundArray[6].length; i++)
	{
		score = roundArray[6][i].endtime - roundArray[6][i].starttime;
		if (score < 300) { score = 300; }
		if (score > 3000) { score = 3000; }
		incompatible += Math.log(score);
	}
    incompatible /= (roundArray[6].length - 1);
    
    // calculate variance log(RT) for first key trial
    cvar = 0;
	for (i=1; i<roundArray[3].length; i++)
	{
		score = roundArray[3][i].endtime - roundArray[3][i].starttime;
		if (score < 300) { score = 300; }
		if (score > 3000) { score = 3000; }
	    cvar += Math.pow((Math.log(score) - compatible),2);
	}
	
	// calculate variance log(RT) for second key trial
	ivar = 0;
	for (i=1; i<roundArray[6].length; i++)
	{
		score = roundArray[6][i].endtime - roundArray[6][i].starttime;
		if (score < 300) { score = 300; }
		if (score > 3000) { score = 3000; }
	    ivar += Math.pow((Math.log(score) - incompatible),2);
	}
	
	// calculate t-value
	tvalue = (incompatible - compatible) / Math.sqrt(((cvar/39) + (ivar/39))/40);
    
    // determine effect size from t-value and create corresponding text
	if (Math.abs(tvalue) > 2.89) { severity = " <b>much more</b> than "; }
	else if (Math.abs(tvalue) > 2.64) { severity = " <b>more</b> than "; }	
	else if (Math.abs(tvalue) > 1.99) { severity = " <b>a little more</b> than "; }
	else if (Math.abs(tvalue) > 1.66) { severity = " <b>just slightly more</b> than "; }
	else { severity = ""; }
	
	// put together feedback based on direction & magnitude
	if (tvalue < 0 && severity != "")
    { 
        resulttext = "<div style='text-align:center;padding:20px'>You associate "+openA+escapeHtml(template.catB.label)+closeA+" with "+open1+escapeHtml(template.cat1.label)+close1;
        resulttext += " and "+openA+escapeHtml(template.catA.label)+closeA+" with "+open1+escapeHtml(template.cat2.label)+close1+severity;
        resulttext += "you associate "+openA+escapeHtml(template.catA.label)+closeA+" with "+open1+escapeHtml(template.cat1.label)+close1;
        resulttext += " and "+openA+escapeHtml(template.catB.label)+closeA+" with "+open1+escapeHtml(template.cat2.label)+close1+".</div>"; 
        // resulttext += "<div>incompatible: "+incompatible+" ("+(ivar/39)+"); compatible: "+compatible+" ("+(cvar/39)+"); tvalue: "+tvalue+"</div>";
    }
    else if (tvalue > 0 && severity != "")
    { 
        resulttext = "<div style='text-align:center;padding:20px'>You associate "+openA+escapeHtml(template.catA.label)+closeA+" with "+open1+escapeHtml(template.cat1.label)+close1;
        resulttext += " and "+openA+escapeHtml(template.catB.label)+closeA+" with "+open1+escapeHtml(template.cat2.label)+close1+severity;
        resulttext += "you associate "+openA+escapeHtml(template.catB.label)+closeA+" with "+open1+escapeHtml(template.cat1.label)+close1;
        resulttext += " and "+openA+escapeHtml(template.catA.label)+closeA+" with "+open1+escapeHtml(template.cat2.label)+close1+".</div>"; 
        // resulttext += "<div>incompatible: "+incompatible+" ("+(ivar/39)+"); compatible: "+compatible+" ("+(cvar/39)+"); tvalue: "+tvalue+"</div>";
    }
    else
    { 
        resulttext = "<div style='text-align:center;padding:20px'>You do not associate "+openA+escapeHtml(template.catA.label)+closeA;
        resulttext += " with "+open1+escapeHtml(template.cat1.label)+close1+" any more or less than you associate ";
        resulttext += openA+escapeHtml(template.catB.label)+closeA+" with "+open1+escapeHtml(template.cat1.label)+close1+".</div>"; 
        // resulttext += "<div>incompatible: "+incompatible+" ("+(ivar/39)+"); compatible: "+compatible+" ("+(cvar/39)+"); tvalue: "+tvalue+"</div>";
    }
	$("#picture_frame").html(resulttext);
}

// Converts the data for each session and round into a comma-delimited string
// and passes it to saveResult.php to be written by the server
function WriteFile()
{
	
	var subject = sub;
	subject = subject.length==0 ? "unknown" : subject;
	var str = "";
	for (i=0; i<roundArray.length; i++)
	{
		for (j=0;j<roundArray[i].length;j++)
		{
			str += i + "," + j + ",";
	        str += roundArray[i][j].category+",";
			str += roundArray[i][j].catIndex+",";
			str += roundArray[i][j].errors+",";
			str += (roundArray[i][j].endtime - roundArray[i][j].starttime)+"\n";
			var catIndex=roundArray[i][j].catIndex;
			var category=roundArray[i][j].category;
			var datai=i;
			var dataj=j;
			var mseconds=(roundArray[i][j].endtime - roundArray[i][j].starttime);
			
		}
	}
	
	
    $.post("core/saveResult.php", { 'template':template.name, 
 			'subject': subject, 'data': str });	
 	
	// notify user of success?
}


// This monitors for keyboard events
function keyHandler(kEvent)
{   
	// move from instructions to session on spacebar press
	var unicode;
	if (!kEvent) var kEvent = window.event;
	if (kEvent.keyCode) unicode = kEvent.keyCode;
	else if (kEvent.which) unicode = kEvent.which;
	if (currentState == "instruction" && unicode == 32)
    {
		currentState = "play";
		$('#exp_instruct').html('');
		displayItem();
    }
	// in session
	if (currentState == "play")
	{
		runSession(kEvent);
	}
}

// Get the stimulus for this session & round and display it
function displayItem()
{
	var tRound = roundArray[session][roundnum]; 
	tRound.starttime = new Date().getTime(); // the time the item was displayed
	if (tRound.itemtype == "img")
	{
		if (tRound.category == template.catA.datalabel)
			{ $("#"+template.catA.datalabel+tRound.catIndex).css("display","block"); }
		else if (tRound.category == template.catB.datalabel)
			{ $("#"+template.catB.datalabel+tRound.catIndex).css("display","block"); }
		else if (tRound.category == template.cat1.datalabel)
			{ $("#"+template.cat1.datalabel+tRound.catIndex).css("display","block"); }
		else if (tRound.category == template.cat2.datalabel)
			{ $("#"+template.cat2.datalabel+tRound.catIndex).css("display","block"); }
	}
	else if (tRound.itemtype == "txt")
	{
		if (tRound.category == template.catA.datalabel)
		{ 
			$("#word").html(openA+escapeHtml(template.catA.items[tRound.catIndex])+closeA)
			$("#word").css("display","block"); 
		}
		else if (tRound.category == template.catB.datalabel)
		{ 
			$("#word").html(openA+escapeHtml(template.catB.items[tRound.catIndex])+closeA)
			$("#word").css("display","block"); 
		}
		else if (tRound.category == template.cat1.datalabel)
		{ 
			$("#word").html(open1+escapeHtml(template.cat1.items[tRound.catIndex])+close1)
			$("#word").css("display","block"); 
		}
		else if (tRound.category == template.cat2.datalabel)
		{ 
			$("#word").html(open1+escapeHtml(template.cat2.items[tRound.catIndex])+close1)
			$("#word").css("display","block"); 
		}
	}
}

function runSession(kEvent)
{
	var rCorrect = roundArray[session][roundnum].correct;
	var unicode = kEvent.keyCode? kEvent.keyCode : kEvent.charCode;
	keyE = (unicode == 69 || unicode == 101 );
	keyI = (unicode == 73 || unicode == 105 );
	
	// if correct key (1 & E) or (2 & I)
	if ((rCorrect == 1 && keyE) || (rCorrect == 2 && keyI))
	{
		$("#wrong").css("display","none"); // remove X if it exists
		roundArray[session][roundnum].endtime = new Date().getTime(); // end time
		// if more rounds
		if (roundnum < roundArray[session].length-1)
		{
			roundnum++;
			$(".IATitem").css("display","none"); // hide all items
			displayItem(); // display chosen item
		}
		else
		{
    		$(".IATitem").css("display","none"); // hide all items
			currentState = "instruction"; // change state to instruction
			session++; // move to next session
			roundnum=0; // reset rounds to 0
		    instructionPage(); // show instruction page
		}
	}
	// incorrect key
	else if ((rCorrect == 1 && keyI) || (rCorrect == 2 && keyE))
	{
		$("#wrong").css("display","block"); // show X
		roundArray[session][roundnum].errors++; // note error
	}
}
