package com.logicaldoc.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.CharacterCodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

/**
 * Some utility methods specialized in string manipulation
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 4.5
 */
public class StringUtil {

	/**
	 * Splits a string into tokens separated by a separator
	 * 
	 * @param src The source string
	 * @param separator The separator character
	 * @param tokenSize Size or each token
	 * @return
	 */
	public static String split(String src, char separator, int tokenSize) {
		StringBuffer sb = new StringBuffer();
		String[] tokens = split(src, tokenSize);
		for (int i = 0; i < tokens.length; i++) {
			if (sb.length() > 0)
				sb.append(separator);
			sb.append(tokens[i]);
		}
		return sb.toString();
	}

	/**
	 * Splits a string into an array of tokens
	 * 
	 * @param src The source string
	 * @param tokenSize size of each token
	 * @return
	 */
	public static String[] split(String src, int tokenSize) {
		ArrayList<String> buf = new ArrayList<String>();
		for (int i = 0; i < src.length(); i += tokenSize) {
			int j = i + tokenSize;
			if (j > src.length())
				j = src.length();
			buf.add(src.substring(i, j));
		}
		return buf.toArray(new String[] {});
	}

	/**
	 * Writes to UFT-8 encoding.
	 */
	public static String writeToString(Reader reader) throws IOException {
		return writeToString(reader, "UTF-8");
	}

	/**
	 * Writes the content from the reader in a string encoded as specified.
	 * 
	 * @param reader Attention, this will be closed at the end of invocation
	 * @param targetEncoding The output string encoding
	 * @return The encoded string
	 * @throws IOException
	 */
	public static String writeToString(Reader reader, String targetEncoding) throws IOException {
		String enc = "UTF-8";
		if (StringUtils.isNotEmpty(targetEncoding))
			enc = targetEncoding;

		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		OutputStreamWriter osw = null;
		try {
			baos = new ByteArrayOutputStream();
			osw = new OutputStreamWriter(baos, enc);
			BufferedWriter bw = new BufferedWriter(osw);
			BufferedReader br = new BufferedReader(reader);
			String inputLine;
			while ((inputLine = br.readLine()) != null) {
				bw.write(inputLine);
				bw.newLine();
			}
			bw.flush();
			osw.flush();
			return new String(baos.toByteArray(), enc);
		} finally {
			try {
				if (reader != null)
					reader.close();
				if (osw != null)
					osw.close();
			} catch (IOException e) {
			}
		}
	}

	public static String writeToString(InputStream is, String targetEncoding) throws IOException {
		String enc = "UTF-8";
		if (StringUtils.isNotEmpty(targetEncoding))
			enc = targetEncoding;

		Writer writer = new StringWriter();
		char[] buffer = new char[1024];
		try {
			Reader reader = new BufferedReader(new InputStreamReader(is, enc));
			int n;
			while ((n = reader.read(buffer)) != -1) {
				writer.write(buffer, 0, n);
			}
		} finally {
			if (is != null)
				is.close();
		}
		return writer.toString();
	}

	public static String arrayToString(Object[] a, String separator) {
		String result = "";
		if (a.length > 0) {
			result = a[0].toString(); // start with the first element
			for (int i = 1; i < a.length; i++) {
				result = result + separator + a[i];
			}
		}
		return result;
	}
	
	public static String collectionToString(Collection<?> collection, String separator) {
		return String.join(separator, collection.stream().map(o -> o.toString()).collect(Collectors.toList()));
	}

	public static String removeNonUtf8Chars(String src) throws CharacterCodingException {
		return src.replace('\uFFFF', ' ').replace('\uD835', ' ');
	}
}