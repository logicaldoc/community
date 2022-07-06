package com.logicaldoc.core.searchengine;

import java.util.HashSet;
import java.util.Set;

/**
 * Models the field names that can be stored in the index
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public enum HitField {
	ID("id"), FILENAME("fileName"), TITLE("title"), FOLDER_ID("folderId"), CONTENT("content"), TAGS(
			"tags"), TEMPLATE_ID("templateId"), FOLDER_NAME("folderName"), CREATION("creation"), DATE("date"), SIZE(
					"size"), CUSTOM_ID("customId"), DOC_REF(
							"docRef"), COMMENT("comment"), LANGUAGE("language"), TENANT_ID("tenantId"), NOTES("notes");

	private final String name;

	private HitField(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public static Set<HitField> all() {
		Set<HitField> fields = new HashSet<HitField>();
		fields.add(CONTENT);
		fields.add(ID);
		fields.add(FILENAME);
		fields.add(TITLE);
		fields.add(FOLDER_ID);
		fields.add(TAGS);
		fields.add(TEMPLATE_ID);
		fields.add(FOLDER_NAME);
		fields.add(CREATION);
		fields.add(DATE);
		fields.add(CUSTOM_ID);
		fields.add(DOC_REF);
		fields.add(COMMENT);
		fields.add(LANGUAGE);
		fields.add(TENANT_ID);
		fields.add(NOTES);
		return fields;
	}

	/**
	 * HitField list suitable for searches
	 * 
	 * @return list of fields in a single string representation
	 */
	public static String searchList() {
		Set<HitField> fields = all();
		String buf = fields.toString();
		buf = buf.substring(buf.indexOf(',') + 1, buf.length() - 1) + ",ext_*";
		return buf;
	}

	@Override
	public String toString() {
		return name;
	}
}