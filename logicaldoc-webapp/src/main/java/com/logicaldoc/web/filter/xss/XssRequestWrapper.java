package com.logicaldoc.web.filter.xss;

import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.owasp.esapi.ESAPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This wrapper implements the checks for avoiding the Cross Site Scripting
 * (XSS).
 * 
 * <br>
 * See <a href=
 * "https://www.owasp.org/index.php/XSS">https://www.owasp.org/index.php/XSS</a>
 * <br>
 * See <a href=
 * "https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html">https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html</a>
 * *
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 *
 */
public class XssRequestWrapper extends HttpServletRequestWrapper {

	protected static Logger log = LoggerFactory.getLogger(XssRequestWrapper.class);

	public XssRequestWrapper(HttpServletRequest servletRequest) {
		super(servletRequest);
	}

	@Override
	public String[] getParameterValues(String parameter) {
		String[] values = super.getParameterValues(parameter);

		if (values == null)
			return new String[0];

		int count = values.length;
		String[] encodedValues = new String[count];
		for (int i = 0; i < count; i++) {
			encodedValues[i] = stripXSS(values[i]);
		}

		return encodedValues;
	}

	@Override
	public String getParameter(String parameter) {
		String value = super.getParameter(parameter);
		if (value != null && "tenant".equals(parameter)) {
			// Check that the content of this special parameter is a tenant name
			Pattern scriptPattern = Pattern.compile("[^a-z^0-9^\\-]", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");
		} else if (value != null && ("version".equals(parameter) || "fileVersion".equals(parameter))) {
			// Check that the content of this special parameter is a version
			Pattern scriptPattern = Pattern.compile("[^0-9^.]", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");
		} else if (value != null && "locale".equals(parameter)) {
			// Check that the content of this special parameter is a locale
			Pattern scriptPattern = Pattern.compile("[^a-z\\-]", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");
		} else if (value != null && "control".equals(parameter)) {
			// Check that the content of this special parameter is a control
			Pattern scriptPattern = Pattern.compile("[\\W]", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");
		}
		value = stripXSS(value);
		return value;
	}

	@Override
	public String getHeader(String name) {
		String value = super.getHeader(name);
		return stripXSS(value);
	}

	private String stripXSS(String value) {
		if (value != null) {
			// NOTE: It's highly recommended to use the ESAPI library and
			// uncomment the following line to
			// avoid encoded attacks.
			value = ESAPI.encoder().canonicalize(value);

			// Avoid null characters
			value = value.replace("", "");

			// Avoid "; sequences
			Pattern scriptPattern = Pattern.compile("\";", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid '; sequences
			scriptPattern = Pattern.compile("';", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid "); sequences
			scriptPattern = Pattern.compile("\"\\);", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid '); sequences
			scriptPattern = Pattern.compile("'\\);", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid anything between script tags
			scriptPattern = Pattern.compile("<script>(.*?)</script>", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid anything in a src='...' type of expression
			scriptPattern = Pattern.compile("src[\r\n]*=[\r\n]*\\\"(.*?)\\\"",
					Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL);
			value = scriptPattern.matcher(value).replaceAll("");

			// Remove any lonesome </script> tag
			scriptPattern = Pattern.compile("</script>", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");

			// Remove any lonesome <script ...> tag
			scriptPattern = Pattern.compile("<script(.*?)>",
					Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid eval(...) expressions
			scriptPattern = Pattern.compile("eval\\((.*?)\\)",
					Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid expression(...) expressions
			scriptPattern = Pattern.compile("expression\\((.*?)\\)",
					Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid javascript:... expressions
			scriptPattern = Pattern.compile("javascript:", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid vbscript:... expressions
			scriptPattern = Pattern.compile("vbscript:", Pattern.CASE_INSENSITIVE);
			value = scriptPattern.matcher(value).replaceAll("");

			// Avoid onload= expressions
			scriptPattern = Pattern.compile("onload(.*?)=",
					Pattern.CASE_INSENSITIVE | Pattern.MULTILINE | Pattern.DOTALL);
			value = scriptPattern.matcher(value).replaceAll("");
		}
		return value;
	}
}