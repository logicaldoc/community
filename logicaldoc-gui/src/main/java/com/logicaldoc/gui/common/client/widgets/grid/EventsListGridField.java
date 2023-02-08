package com.logicaldoc.gui.common.client.widgets.grid;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * List grid field to display the events related to a specific subscription or
 * other events-related object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class EventsListGridField extends ColoredListGridField {

	public EventsListGridField(String name, String title) {
		super(name, title);
		setup();
	}

	public EventsListGridField(String name) {
		super(name);
		setup();
	}

	public EventsListGridField() {
		super("events", "notifyon");
		setup();
	}

	private void setup() {
		setCanEdit(false);
		setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			try {
				if (value != null && !value.toString().isEmpty()) {
					return decodeEvents(value, rec);
				} else
					return "";
			} catch (Throwable e) {
				return "";
			}
		});
	}

	private String decodeEvents(Object value, ListGridRecord rec) {
		String decoded = "document";
		// Translate the set of events
		String[] key = null;

		if (!value.toString().contains(","))
			key = new String[] { value.toString().trim() };
		else
			key = value.toString().split(",");
		List<String> labels = new ArrayList<>();
		for (String string : key) {
			if (string.trim().isEmpty())
				continue;
			labels.add(I18N.message(string.trim() + ".short"));
		}

		String str = labels.toString().substring(1);
		decoded = str.substring(0, str.length() - 1);

		String colorSpec = rec.getAttributeAsString("color");
		if (colorSpec != null && !colorSpec.isEmpty())
			return "<span style='color: " + colorSpec + ";'>" + decoded + "</span>";
		else
			return decoded != null ? decoded : "";
	}

}