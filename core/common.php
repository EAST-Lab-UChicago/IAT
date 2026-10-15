<?php
// Shared helpers for fileManager.php and saveResult.php

define('IAT_TEMPLATE_DIR', realpath(__DIR__ . '/../templates'));

// Template names become directory names, so only allow letters, digits,
// spaces, underscores, hyphens and parentheses (max 30 bytes, as before)
function valid_template_name($name)
{
	return is_string($name)
		&& strlen($name) > 0
		&& strlen($name) <= 30
		&& preg_match('/^[\p{L}\p{N} _()\-]+$/u', $name) === 1
		&& trim($name) === $name;
}

// Absolute path of an existing template directory, or false if the name is
// invalid or the directory does not resolve inside templates/
function template_path($name)
{
	if (!valid_template_name($name) || IAT_TEMPLATE_DIR === false)
	{
		return false;
	}
	$path = realpath(IAT_TEMPLATE_DIR . '/' . $name);
	if ($path === false || !is_dir($path) || dirname($path) !== IAT_TEMPLATE_DIR)
	{
		return false;
	}
	return $path;
}

// Image item names are plain file names inside a template's img/ folder
function valid_image_name($name)
{
	return is_string($name)
		&& strlen($name) <= 100
		&& $name === basename($name)
		&& preg_match('/^[\p{L}\p{N} _()\-][\p{L}\p{N} _.()\-]*\.(jpe?g|png|gif|bmp|webp)$/iu', $name) === 1;
}

function send_text_headers()
{
	header('Content-Type: text/plain; charset=utf-8');
	header('X-Content-Type-Options: nosniff');
}
?>
