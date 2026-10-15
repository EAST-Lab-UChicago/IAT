<?php
require_once(__DIR__ . '/common.php');
require_once(__DIR__ . '/auth.php');

send_text_headers();
header('X-Frame-Options: DENY');
require_experimenter();

if ($_SERVER['REQUEST_METHOD'] !== 'POST')
{
	http_response_code(405);
	echo "Error -2: POST required";
	exit;
}
require_same_origin();

$newTemplate = array(
	"name" => "",
	"showResult" => "true",
	"IATtype" => "two",
	"catA" => array(
		"label" => "",
		"datalabel" => "",
		"itemtype" => "txt",
		"items" => array()
	),
	"catB" => array(
		"label" => "",
		"datalabel" => "",
		"itemtype" => "txt",
		"items" => array()
	),
	"cat1" => array(
		"label" => "",
		"datalabel" => "",
		"itemtype" => "txt",
		"items" => array()
	),
	"cat2" => array(
		"label" => "",
		"datalabel" => "",
		"itemtype" => "txt",
		"items" => array()
	)
);

// Maximum item index accepted per category
define('MAX_ITEMS', 500);

function active_file()
{
	return IAT_TEMPLATE_DIR . '/active.txt';
}

function read_active()
{
	$active = json_decode(@file_get_contents(active_file()));
	if (!is_object($active))
	{
		$active = new stdClass();
		$active->active = 'Empty';
	}
	return $active;
}

//update active and available templates
function updateActive($optArr=NULL)
{
	$fh = fopen(active_file(), 'c+');
	if ($fh === false)
	{
		return false;
	}
	flock($fh, LOCK_EX);
	$active = json_decode(stream_get_contents($fh));
	if (!is_object($active))
	{
		$active = new stdClass();
		$active->active = 'Empty';
	}

	// When "op" = "setActive", calls updateActive with 'newActive' option set
	if(isset($optArr) && isset($optArr['newActive']))
	{
		$active->active = $optArr['newActive'];
	}
	else if(isset($optArr['op']) && $optArr['op'] == 'saveTemplate')
	{
		if($active->active == $optArr['oldName'])
		{
			$active->active = $optArr['newName'];
		}
	}
	else if(isset($optArr['op']) && $optArr['op'] == 'deleteTemplate')
	{
		if($active->active == $optArr['oldName'])
		{
			$active->active = 'Empty';
		}
	}

	$available = array();
	if ($handle = opendir(IAT_TEMPLATE_DIR))
	{
	    while (false !== ($file = readdir($handle))) {
	        if (template_path($file) !== false) {
	            array_push($available,$file);
	        }
	    }
	    closedir($handle);
	}
	natcasesort($available);
	$active->available = array_values($available);

	ftruncate($fh, 0);
	rewind($fh);
	fwrite($fh, json_encode($active));
	fflush($fh);
	flock($fh, LOCK_UN);
	fclose($fh);
	return true;
}

// Returns $form[$key] if it is a string, otherwise null
function form_str($form, $key)
{
	return (isset($form[$key]) && is_string($form[$key])) ? $form[$key] : null;
}

function error_item($html)
{
	return "<li class='error-item'>" . $html . "</li>";
}

function h($str)
{
	return htmlspecialchars((string)$str, ENT_QUOTES, 'UTF-8');
}

// Recursively deletes a template directory (only one level of subfolders)
function delete_template_dir($folder_dir)
{
	foreach (array('img', 'output') as $sub)
	{
		$subdir = $folder_dir . '/' . $sub;
		if (is_dir($subdir) && !is_link($subdir))
		{
			foreach (scandir($subdir) as $item) {
			    if ($item == '.' || $item == '..') continue;
			    $path = $subdir . '/' . $item;
			    if (is_file($path) || is_link($path)) unlink($path);
			}
			rmdir($subdir);
		}
	}
	if (is_file($folder_dir . '/input.txt'))
	{
		unlink($folder_dir . '/input.txt');
	}
	return @rmdir($folder_dir);
}


// High level interface: process command sent from javascript
$op = isset($_POST['op']) && is_string($_POST['op']) ? $_POST['op'] : null;
if( $op !== null )
{
	switch($op)
	{
		case 'exists':
			if( isset($_POST['template']) && is_string($_POST['template']) )
			{
				if( isset($_POST['files']) && is_array($_POST['files']) )
				{
					$template = $_POST['template'];
					$template_dir = template_path($template);
					$newfiles = array();
					foreach ( $_POST['files'] as $file )
					{
						if ($template_dir !== false && valid_image_name($file) &&
							is_file($template_dir . "/img/" . $file))
						{
							array_push($newfiles, "templates/" . $template . "/img/" . $file);
						}
						else
						{
							array_push($newfiles, "core/no-image.jpg");
						}
					}
					echo implode(",",$newfiles);
				}
				else
				{
					echo "Error 1.1: files to check not specified";
				}
			}
			else
			{
				echo "Error 1.0: template not specified";
			}
			break;
		case 'setActive':
			if( isset($_POST['template']) && is_string($_POST['template']) )
			{
				$template_dir = template_path($_POST['template']);
				if ($template_dir === false || !is_file($template_dir . '/input.txt'))
				{
					echo "Error 3.1: template does not exist";
				}
				else
				{
					$optArr = array('newActive' => $_POST['template']);
					updateActive($optArr);
					echo "success";
				}
			}
			else
			{
				echo "Error 3.0: template not specified";
			}
			break;
		case 'saveTemplate':
			if( isset($_POST['template']) )
			{
				if( isset($_POST['oldname']) && is_string($_POST['oldname']) &&
					isset($_POST['form']) && is_string($_POST['form']) )
				{
					$oldname = $_POST['oldname'];
					$old_dir = template_path($oldname);
					$newform = $newTemplate;
					$form = array();
					parse_str($_POST['form'], $form);
				 	if (!is_array($form) || count($form) == 0) {
				        echo "Error 2.2: form data not properly formatted";
				    }
					else if ($old_dir === false)
					{
						echo "Error 2.3: template does not exist";
					}
					else {
						$errors = "";
						$template_name = form_str($form, "template-name");
						$name_ok = false;
						// template name is set, is < 30 characters, and is safe to use as a folder name
						if(!valid_template_name($template_name))
						{
							$errors .= error_item("Please input a valid template name (less than 30 characters)");
						}
						else
						{
							if ($template_name == "Empty")
							{
								$errors .= error_item("<div style='color:red'>'Empty' is a reserved Template name</div>");
							}
							// if new name != old name, and new name already taken, return error
							if ($oldname != $template_name && file_exists(IAT_TEMPLATE_DIR."/".$template_name))
							{
								$errors .= error_item("<div style='color:red'>Template name " . h($template_name) . " is taken.</div>");
							}
							else
							{
								$newform["name"] = $template_name;
								$name_ok = true;
							}
						}
						// IAT type
						$iat_type = form_str($form, "IAT-type");
						if($iat_type === null)
						{
							$errors .= error_item("IAT type not set");
						}
						else if (in_array($iat_type, array("one", "two"), true))
						{
							$newform["IATtype"] = $iat_type;
						}
						// show result
						$show_results = form_str($form, "show-results");
						if($show_results === null)
						{
							$errors .= error_item("Show results not set");
						}
						else if (in_array($show_results, array("show", "noshow"), true))
						{
							$newform["showResult"] = $show_results;
						}
						// for each category
						$newfiles = array();
						$catnames = array("A","B","1","2");
						for ($i=1;$i<=4;$i++)
						{
							$newfiles["tabs".$i] = array();
							$cat = "cat".$catnames[$i-1];
							// if category is visible
							if(form_str($form, "tabs".$i."-shown") == "true")
							{
								// category name
								$catlabel = form_str($form, "tabs".$i."-catlabel-input");
								if($catlabel === null ||
									strlen($catlabel) > 30 ||
									$catlabel != preg_replace('/[^\p{L}\p{N}\-\(\)\$\#\@\? ]+/u', '', $catlabel))
								{
									$errors .= error_item("Please input a valid category label for Category " . $catnames[$i-1]);
								}
								else
								{
									// add category label0
									$newform[$cat]["label"] = $catlabel;
								}
								// data label
								$datalabel = form_str($form, "tabs".$i."-datalabel-input");
								if($datalabel === null ||
									strlen($datalabel) > 3 ||
									$datalabel != preg_replace('/[^\p{L}\p{N}]+/u','',$datalabel))
								{
									$errors .= error_item("Please input a valid data label for Category " . $catnames[$i-1] . " (less than 3 alphanumeric characters)");
								}
								else
								{
									// add data label
									$newform[$cat]["datalabel"] = $datalabel;
								}
								// text or images
								if(!isset($form["tabs".$i."-txt"]) && !isset($form["tabs".$i."-img"]))
								{
									$errors .= error_item("Text or images not set for category " . $catnames[$i-1]);
								}
								else
								{
									if(isset($form["tabs".$i."-txt"]))
									{
										$newform[$cat]["itemtype"] = "txt";
										$txtorimg = "txt";
									}
									else
									{
										$newform[$cat]["itemtype"] = "img";
										$txtorimg = "img";
									}
									// get max id number for items
									$max_item = form_str($form, "max-".$cat);
									if($max_item === null || !preg_match('/^[0-9]+$/', $max_item))
									{
										$errors .= error_item("Maximum item number not set for category " . $catnames[$i-1]);
									}
									else
									{
										$max_item = min((int)$max_item, MAX_ITEMS);
										// for each item
										for ($j=0; $j<=$max_item; $j++)
										{
											// some values of $j might not exist if items deleted
											if(!array_key_exists($cat.$j."-txt", $form))
											{
												continue;
											}
											$item = form_str($form, $cat.$j."-txt");
											if ($item === null)
											{
												$item = "";
											}
											// if text
											if($txtorimg == "txt")
											{
												// check if has ".jpg", etc.
												if( preg_match("/\.[\p{L}]{2,4}/u",$item))
												{
													$errors .= error_item("The text input, '" . h($item) . "' in Category " . $catnames[$i-1] . " appears to be a file name.");
												}
												else if ( preg_match("/[^\p{L}\p{N}\-\(\)\$\#\@\? ]+/u", $item))
												{
													$errors .= error_item("The text input, '" . h($item) . "' in Category " . $catnames[$i-1] . " is not valid.");
												}
												else
												{
													array_push($newform[$cat]["items"], $item);
												}
											}
											// if images
											else
											{
												// verify image name is safe and the image exists
												$imgfile = "templates/" . $oldname . "/img/" . $item;
												if( !valid_image_name($item) ||
													($oldname != "Empty" && !is_file($old_dir . "/img/" . $item)))
												{
													$errors .= error_item(h($item) . " in Category ".$catnames[$i-1]." is not a valid file name.");
													array_push($newfiles["tabs".$i], "core/no-image.jpg");
												}
												else
												{
													array_push($newfiles["tabs".$i], $imgfile);
													array_push($newform[$cat]["items"], $item);
												}
											}
										}
									}
								}
							}
						}

						// if no errors
						if(strlen($errors) == 0 && $name_ok)
						{
							// encode object as JSON string
							$json_template = json_encode($newform);
							$new_dir = IAT_TEMPLATE_DIR . "/" . $template_name;

							// if new template
							if ($oldname == "Empty")
							{
								// create new folders
								mkdir($new_dir);
								mkdir($new_dir."/img");
								mkdir($new_dir."/output");
							}
							// else if template name has changed
							else if ($oldname != $template_name)
							{
								// checked for template-name conflict happened earlier, so this is safe
								rename($old_dir, $new_dir);
							}
							file_put_contents($new_dir."/input.txt", $json_template, LOCK_EX);

							if ($oldname == "Empty")
							{
								// add new template to available list
								updateActive();
							}
							else if ($oldname != $template_name)
							{
								// check if old name is currently active, if so, change
								// also need to rename in active.txt
								$optArr = array('oldName'	=> $oldname,
												'newName'	=> $template_name,
												'op'		=> 'saveTemplate');
								updateActive($optArr);
							}
							echo $json_template;
						}
						// else return errors
						else
						{
							$images = array();
							foreach($newfiles as $key => $value)
							{
								$images[$key] = implode(",",$value);
							}
							echo json_encode(array("images" => $images, "errors" => $errors));
						}
					}
				}
				else
				{
					echo "Error 2.1: old name or form not specified";
				}
			}
			else
			{
				echo "Error 2.0: template not specified";
			}
			break;
		case 'deleteTemplate':
			if( isset($_POST['template']) && is_string($_POST['template']) )
			{
				$template = $_POST['template'];
				$folder_dir = template_path($template);
				$active = read_active();

				if( $folder_dir === false )
				{
					echo "Error 5.1: template does not exist";
				}
				else if ($template == "Empty")
				{
					echo "Error 5.2: 'Empty' is a reserved Template name and cannot be deleted";
				}
				else if (isset($active->active) && $active->active == $template)
				{
					echo "Error 5.3: " . $template . " is currently active.  Please make another template active before deleting this template.";
				}
				else
				{
					delete_template_dir($folder_dir);

					$optArr = array('oldName'	=> $template,
									'op'		=> 'deleteTemplate');
					updateActive($optArr);
				}
			}
			else
			{
				echo "Error 6.0: template name not specified";
			}
			break;
		default:
			echo "Error -1: unknown operation";
			break;
	}
}
else
{
	echo "Error -1: no operation specified";
}

?>
