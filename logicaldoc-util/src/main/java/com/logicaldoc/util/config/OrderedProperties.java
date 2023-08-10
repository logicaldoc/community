package com.logicaldoc.util.config;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;
import java.util.TreeSet;

/**
 * This is an extension of Java Properties that stores the properties
 * alphabetically
 * 
 * @author Brian Pipa - http://pipasoft.com
 * @version 1.0
 */
public class OrderedProperties extends Properties {

	private static final long serialVersionUID = 1L;

	@Override
	public synchronized Enumeration<Object> keys() {
		return Collections.enumeration(new TreeSet<Object>(super.keySet()));
	}

	/**
	 * Overrides the original store() method and sorts the output
	 * 
	 * @param out a FileOutPutStream to send the output to
	 * @param header a textual header for the top of the file
	 * @exception IOException when things go wrong
	 */
	@Override
	public void store(OutputStream out, String header) throws IOException {
		List<String> keys = getKeys();

		// write the header
		DataOutputStream dataoutputstream = new DataOutputStream(out);
		dataoutputstream.writeBytes("#" + header + "\n");

		// write the date/time
		Date now = new Date();
		dataoutputstream.writeBytes("#" + now + "\n");

		// now, loop through and write out the properties
		String oneline;
		String thekey;
		String thevalue;

		for (int i = 0; i < keys.size(); i++) {
			thekey = keys.get(i);
			thevalue = this.getProperty(thekey);
			thevalue = doubleSlash(thevalue);

			oneline = thekey + "=" + thevalue + "\n";
			dataoutputstream.writeBytes(oneline);
		}

		dataoutputstream.flush();
		dataoutputstream.close();
	}

	/**
	 * Private method to double slash paths
	 * 
	 * @param orig the string to double slash
	 * @return a double-slashed string
	 */
	private String doubleSlash(String orig) {
		StringBuilder buf = new StringBuilder();

		for (int i = 0; i < orig.length(); i++) {
			if (orig.charAt(i) == '\\') {
				buf.append("\\\\");
			} else {
				buf.append(orig.charAt(i));
			}
		}

		return buf.toString();
	}

	/**
	 * All the keys but alphabetically ordered
	 * 
	 * @return the ordered collection of all the keys
	 */
	public List<String> getKeys() {
		ArrayList<String> keys = new ArrayList<>();

		for (Object key : keySet())
			keys.add(key.toString());

		// sort them
		Collections.sort(keys);

		return keys;
	}
}