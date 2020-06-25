package com.logicaldoc.util.html;

import org.apache.commons.lang.StringEscapeUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Document.OutputSettings;
import org.jsoup.safety.Whitelist;

/**
 * Removes elements from an HTML document that may lead to security problems
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 *
 */
public class HTMLSanitizer {

	public static String sanitize(String unsafeHtmlContent) {
		Whitelist whiteList = Whitelist.relaxed().preserveRelativeLinks(true);
		whiteList = whiteList.addTags("head", "html", "style", "body", "fieldsMap", "area");
		whiteList = whiteList.addAttributes(":all", "name", "class", "style", "id", "src", "type", "cellpadding",
				"cellspacing", "alt", "title", "shape", "coords", "width", "height", "dir");
		whiteList = whiteList.addProtocols("img", "src", "http", "https", "data", "cid");

		return Jsoup.clean(unsafeHtmlContent, whiteList);
	}

	public static String sanitizeSimpleText(String unsafeHtmlContent) {
		OutputSettings outputSettings = new OutputSettings().indentAmount(0).prettyPrint(false);
		Whitelist whiteList = Whitelist.simpleText().preserveRelativeLinks(false);
		String sanitized = Jsoup.clean(unsafeHtmlContent, "", whiteList, outputSettings);
		sanitized = StringEscapeUtils.unescapeHtml(sanitized);
		return sanitized;
	}

	public static String stripIframes(String htmlContent) {
		Document doc = Jsoup.parse(htmlContent, "UTF-8");
		doc.select("iframe").remove();
		return doc.text();
	}
}
