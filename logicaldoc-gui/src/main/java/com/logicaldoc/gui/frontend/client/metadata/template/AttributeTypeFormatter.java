package com.logicaldoc.gui.frontend.client.metadata.template;

import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A formatter used to format a grid cell that contains an attribute's type @see
 * {@link GUIAttribute}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class AttributeTypeFormatter implements CellFormatter {
	@Override
	public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
		if (value == null)
			return "";
		int intValue = Integer.parseInt(value.toString());
		switch (intValue) {
		case GUIAttribute.TYPE_STRING:
			return I18N.message("string");
		case GUIAttribute.TYPE_INT:
			return I18N.message("integer");
		case GUIAttribute.TYPE_DOUBLE:
			return I18N.message("decimal");
		case GUIAttribute.TYPE_DATE:
			return I18N.message("date");
		case GUIAttribute.TYPE_BOOLEAN:
			return I18N.message("boolean");
		case GUIAttribute.TYPE_USER:
			return I18N.message("user");
		case GUIAttribute.TYPE_FOLDER:
			return I18N.message("folder");
		case GUIAttribute.TYPE_DOCUMENT:
			return I18N.message("document");
		case GUIAttribute.TYPE_SECTION:
			return I18N.message("section");	
		default:
			return value.toString();
		}

	}

	/**
	 * Converts an attribute's type into the human readable label
	 * 
	 * @param type the attribute's type
	 * 
	 * @return the table indicating the type using the current locale
	 */
	public static String format(int type) {
		String value = "";
		switch (type) {
		case GUIAttribute.TYPE_STRING:
			return I18N.message("string");
		case GUIAttribute.TYPE_INT:
			return I18N.message("integer");
		case GUIAttribute.TYPE_DOUBLE:
			return I18N.message("decimal");
		case GUIAttribute.TYPE_DATE:
			return I18N.message("date");
		case GUIAttribute.TYPE_BOOLEAN:
			return I18N.message("boolean");
		case GUIAttribute.TYPE_USER:
			return I18N.message("user");
		case GUIAttribute.TYPE_FOLDER:
			return I18N.message("folder");
		case GUIAttribute.TYPE_DOCUMENT:
			return I18N.message("document");			
		default:
			return value;
		}
	}
}