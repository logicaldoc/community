package com.logicaldoc.util;

/**
 * Useful class used to filter snippets from lucene, excluding those characters
 * that invalidate HTML page
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class SnippetStripper {

	// Lucene font hilight start tag
	private static final String LUCENE_HILIGHT_START = "&lt;font style='background-color:#FFFF00'&gt;";

	// Lucene font hilight end tag
	private static final String LUCENE_HILIGHT_STOP = "&lt;/font&gt;";

	private SnippetStripper() {
	}

	/**
	 * Strips all characters from the input string that may invalidate XML.
	 * Particularly useful for search result summaries
	 * 
	 * @param snippet the string to process
	 * 
	 * @return the sanitized string
	 */
	public static String strip(String snippet) {
		String summary = snippet;
		// Escape all tag delimiters to avoid bad markup in the results page
		summary = summary.replace("<", "&lt;");
		summary = summary.replace(">", "&gt;");

		// But preserve Lucene hilights
		summary = summary.replace(LUCENE_HILIGHT_START, "<font style='background-color:#FFFF00'>");
		summary = summary.replace(LUCENE_HILIGHT_STOP, "</font>");
		String outString = summary;

		// Remove all control characters
		outString = outString.replace("[\\u0000-\\u0020]", " ");
		outString = outString.replace("\\u007F", " ");
		outString = outString.replace("[\\u0080-\\u009F]", " ");

		return outString;
	}
}