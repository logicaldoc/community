package com.logicaldoc.util;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;

/**
 * Utility class for working with encodings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public abstract class Encodings {
	
	private Encodings() {
		// Do nothing
	}
	
	public static List<String> enlistSupportedEncodings() {
		List<String> encodings = new ArrayList<>();

		SortedMap<String, Charset> m = Charset.availableCharsets();
		Set<String> k = m.keySet();
		Iterator<String> i = k.iterator();
		while (i.hasNext()) {
			String n = i.next();
			encodings.add(n);
		}

		return encodings;
	}
}