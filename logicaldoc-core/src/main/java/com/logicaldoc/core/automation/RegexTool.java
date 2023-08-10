package com.logicaldoc.core.automation;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Various Regex-based APIs to make it easy to manipulate regular expressions
 * from Automation.
 *
 * @since 8.7.4
 */
@AutomationDictionary
public class RegexTool {
	
	/**
	 * Result of a Regex search.
	 */
	public class RegexResult {
		/**
		 * @see #getStart()
		 */
		private int start;

		/**
		 * @see #getEnd()
		 */
		private int end;

		/**
		 * @see #getGroup()
		 */
		private String group;

		/**
		 * @param start see {@link #getStart()}
		 * @param end see {@link #getEnd()}
		 * @param group see {@link #getGroup()}
		 */
		public RegexResult(int start, int end, String group) {
			this.start = start;
			this.end = end;
			this.group = group;
		}

		/**
		 * @return the captured group
		 */
		public String getGroup() {
			return this.group;
		}

		/**
		 * @return the capture group's start position
		 */
		public int getStart() {
			return this.start;
		}

		/**
		 * @return the capture group's end position
		 */
		public int getEnd() {
			return this.end;
		}
	}

	/**
	 * Finds the matches into a given content using a given regular expression
	 * 
	 * @param content the content to parse
	 * @param regex the regex to look for in the passed content
	 * 
	 * @return empty list if the passed regex doesn't match the content or
	 *         several {@link RegexResult} objects containing the matched
	 *         position and matched content for all capturing groups, the first
	 *         group representing the whole . The first object is represents the
	 *         entire pattern
	 */
	public List<RegexResult> find(String content, String regex) {
		List<RegexResult> result = new ArrayList<>();
		Matcher matcher = Pattern.compile(regex, Pattern.MULTILINE).matcher(content);
		if (matcher.find()) {
			for (int i = 0; i < matcher.groupCount() + 1; i++) {
				result.add(new RegexResult(matcher.start(i), matcher.end(i), matcher.group(i)));
			}
		}
		return result;
	}

	/**
	 * Finds the matches into a given content using a given regular expression
	 * 
	 * @param content the content to parse
	 * @param regex the regular expression to look for in the passed content
	 * 
	 * @return an empty list if the passed regular expression doesn't match the
	 *         content, several {@link RegexResult} objects containing the
	 *         matched position and matched content for all capturing groups and
	 *         sub-groups otherwise
	 */
	public List<List<RegexResult>> findAll(String content, String regex) {
		List<List<RegexResult>> result = new ArrayList<>();
		Matcher matcher = Pattern.compile(regex, Pattern.MULTILINE).matcher(content);
		while (matcher.find()) {
			List<RegexResult> match = new ArrayList<>();
			for (int i = 0; i < matcher.groupCount() + 1; i++) {
				match.add(new RegexResult(matcher.start(i), matcher.end(i), matcher.group(i)));
			}
			result.add(match);
		}
		return result;
	}

	/**
	 * Compiles a regular expression into a java {@code Pattern} object.
	 *
	 * @param regex the textual representation of the regular expression
	 * 
	 * @return the {@code Pattern} object corresponding to the regular
	 *         expression, or {@code null} if the expression is invalid
	 *         
	 * @since 8.7.4
	 */
	public Pattern compile(String regex) {
		try {
			return Pattern.compile(regex);
		} catch (PatternSyntaxException ex) {
			return null;
		}
	}

	/**
	 * Returns a literal pattern <code>String</code> for the specified
	 * <code>String</code>.
	 * <p>
	 * This method produces a <code>String</code> that can be used to create a
	 * <code>Pattern</code> that would match the string <code>s</code> as if it
	 * were a literal pattern.
	 * </p>
	 * Metacharacters or escape sequences in the input sequence will be given no
	 * special meaning.
	 *
	 * @param s The string to be literalized
	 * @return A literal string replacement
	 * 
	 * @since 8.7.4
	 */
	public String quote(String s) {
		return Pattern.quote(s);
	}

	/**
	 * Returns a literal replacement {@code String} for the specified
	 * 
	 * {@code String}. This method produces a {@code String} that will work as a
	 * literal replacement {@code s} in
	 * {@code String#replaceAll(regularExpression, s)}. The {@code String}
	 * produced will match the sequence of characters in {@code s} treated as a
	 * literal sequence. Slashes ('\') and dollar signs ('$') will be given no
	 * special meaning.
	 * 
	 * @param s the string to be literalized
	 * 
	 * @return a literal string replacement
	 * 
	 * @since 8.7.4
	 */
	public String quoteReplacement(String s) {
		return Matcher.quoteReplacement(s);
	}
}
