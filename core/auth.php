<?php
// HTTP Basic Auth for the experimenter pages.
//
// Credentials are checked against the htpasswd file named in core/config.php
// (created by setup.sh). Only bcrypt entries (htpasswd -B) are supported.
// If the web server has already authenticated the user (REMOTE_USER), that
// is accepted as-is.

function iat_auth_fail($status, $message)
{
	http_response_code($status);
	if ($status == 401)
	{
		header('WWW-Authenticate: Basic realm="IAT Experimenter", charset="UTF-8"');
	}
	header('Content-Type: text/plain; charset=utf-8');
	echo $message;
	exit;
}

function iat_basic_credentials()
{
	if (isset($_SERVER['PHP_AUTH_USER']) && isset($_SERVER['PHP_AUTH_PW']))
	{
		return array($_SERVER['PHP_AUTH_USER'], $_SERVER['PHP_AUTH_PW']);
	}
	// CGI / FastCGI: header passed through by the SetEnvIf rule in .htaccess
	foreach (array('HTTP_AUTHORIZATION', 'REDIRECT_HTTP_AUTHORIZATION') as $key)
	{
		if (isset($_SERVER[$key]) && stripos($_SERVER[$key], 'basic ') === 0)
		{
			$decoded = base64_decode(substr($_SERVER[$key], 6), true);
			if ($decoded !== false && strpos($decoded, ':') !== false)
			{
				return explode(':', $decoded, 2);
			}
		}
	}
	return null;
}

function iat_check_htpasswd($file, $user, $pass)
{
	$lines = @file($file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
	if ($lines === false)
	{
		iat_auth_fail(500, "Experimenter login is misconfigured: cannot read the password file. Re-run setup.sh.");
	}
	foreach ($lines as $line)
	{
		$parts = explode(':', trim($line), 2);
		if (count($parts) == 2 && hash_equals($parts[0], $user))
		{
			return password_verify($pass, $parts[1]);
		}
	}
	return false;
}

function require_experimenter()
{
	foreach (array('REMOTE_USER', 'REDIRECT_REMOTE_USER') as $key)
	{
		if (!empty($_SERVER[$key]))
		{
			return;
		}
	}

	$config_file = __DIR__ . '/config.php';
	$config = file_exists($config_file) ? include($config_file) : null;
	if (!is_array($config) || empty($config['htpasswd']))
	{
		iat_auth_fail(503, "Experimenter login is not configured yet. Run setup.sh on the server to create a password.");
	}

	$creds = iat_basic_credentials();
	if ($creds === null || !iat_check_htpasswd($config['htpasswd'], $creds[0], $creds[1]))
	{
		iat_auth_fail(401, "Login required.");
	}
}

// Rejects cross-site requests (browsers send Basic credentials automatically)
function require_same_origin()
{
	$source = null;
	if (!empty($_SERVER['HTTP_ORIGIN']))
	{
		$source = $_SERVER['HTTP_ORIGIN'];
	}
	else if (!empty($_SERVER['HTTP_REFERER']))
	{
		$source = $_SERVER['HTTP_REFERER'];
	}
	if ($source === null)
	{
		return;
	}
	$parts = parse_url($source);
	$host = isset($parts['host']) ? $parts['host'] : '';
	if (isset($parts['port']))
	{
		$host .= ':' . $parts['port'];
	}
	if (!isset($_SERVER['HTTP_HOST']) || strcasecmp($host, $_SERVER['HTTP_HOST']) !== 0)
	{
		iat_auth_fail(403, "Error: cross-origin request rejected");
	}
}
?>
