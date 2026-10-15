<?php
// Participant-facing endpoint: writes one participant's IAT results to
// templates/<template>/output/<template>-<subject>-<Y-m-d-H-s>.txt
require_once(__DIR__ . '/common.php');
send_text_headers();

if ($_SERVER['REQUEST_METHOD'] !== 'POST')
{
	http_response_code(405);
	echo "Error 5.3: POST required";
	exit;
}

$template = isset($_POST['template']) ? $_POST['template'] : null;
if (!is_string($template) || $template === '')
{
	http_response_code(400);
	echo "Error 5.0: template name not specified";
	exit;
}

$template_dir = template_path($template);
if ($template_dir === false || !is_dir($template_dir . '/output'))
{
	http_response_code(400);
	echo "Error 5.2: invalid template";
	exit;
}

$data = isset($_POST['data']) ? $_POST['data'] : null;
// Results are comma-separated lines of data labels and integers
if (!is_string($data) || $data === '' || strlen($data) > 1000000 ||
	preg_match('/^[\p{L}\p{N},.\-\n]+$/u', $data) !== 1)
{
	http_response_code(400);
	echo "Error 5.1: data not provided";
	exit;
}

// Subject IDs become part of the file name: keep only letters, digits and _
$sub = isset($_POST['subject']) && is_string($_POST['subject']) ? $_POST['subject'] : '';
$sub = substr(preg_replace('/[^A-Za-z0-9_]/', '', $sub), 0, 64);
if ($sub === '')
{
	$sub = 'unknown';
}

// Keep the original file name format (IATanalysis*.R splits it on '-').
// Never overwrite: add _2, _3, ... after the seconds field if the name is taken.
$base = $template_dir . '/output/' . $template . '-' . $sub . '-' . date('Y-m-d-H-s');
$fh = false;
for ($n = 1; $n <= 100 && $fh === false; $n++)
{
	$fname = $base . ($n == 1 ? '' : '_' . $n) . '.txt';
	$fh = @fopen($fname, 'x');
}
if ($fh === false)
{
	http_response_code(500);
	echo "Error 5.4: could not write output";
	exit;
}
flock($fh, LOCK_EX);
fwrite($fh, $data);
flock($fh, LOCK_UN);
fclose($fh);
?>
