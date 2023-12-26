/**
 * Filter and other resources to avoid Cross Site Scripting (XSS) attacks.
 * 
 * <p>
 * Cross-Site Scripting (XSS) attacks are a type of injection, in which
 * malicious scripts are injected into otherwise benign and trusted websites.
 * XSS attacks occur when an attacker uses a web application to send malicious
 * code, generally in the form of a browser side script, to a different end
 * user. Flaws that allow these attacks to succeed are quite widespread and
 * occur anywhere a web application uses input from a user within the output it
 * generates without validating or encoding it.
 * </p>
 * <p>
 * An attacker can use XSS to send a malicious script to an unsuspecting user.
 * The end user’s browser has no way to know that the script should not be
 * trusted, and will execute the script. Because it thinks the script came from
 * a trusted source, the malicious script can access any cookies, session
 * tokens, or other sensitive information retained by the browser and used with
 * that site. These scripts can even rewrite the content of the HTML page. For
 * more details on the different types of XSS flaws.
 * </p>
 *
 * <br>
 * See <a href=
 * "https://www.owasp.org/index.php/XSS">https://www.owasp.org/index.php/XSS</a>
 * <br>
 * See <a href=
 * "https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html">https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html</a>
 * 
 * @since 8.7
 */
package com.logicaldoc.web.filter.xss;