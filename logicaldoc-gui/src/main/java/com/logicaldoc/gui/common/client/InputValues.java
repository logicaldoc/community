package com.logicaldoc.gui.common.client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Records the inputs of the user for different items
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class InputValues {

	// Item name - Inputed value
	private static Map<String, List<Object>> savedInputs = new HashMap<>();

	public static void saveInput(String name, Object value) {
		if (value == null || !Session.get().getConfigAsBoolean("gui.saveinputs"))
			return;

		List<Object> inputs = savedInputs.get(name);
		if (inputs == null) {
			inputs = new ArrayList<>();
			savedInputs.put(name, inputs);
		}
		if (!inputs.contains(value))
			inputs.add(value);
	}

	public static List<Object> getInputs(String name) {
		List<Object> inputs = savedInputs.get(name);
		if (inputs == null)
			inputs = new ArrayList<>();
		return inputs;
	}

	public static List<String> getInputsAsStrings(String name) {
		List<Object> inputs = savedInputs.get(name);
		List<String> str = new ArrayList<>();
		if (inputs != null) {
			for (Object val : inputs) {
				str.add((String) val.toString());
			}
			return str;
		}
		return str;
	}
}