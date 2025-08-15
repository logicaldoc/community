package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;

/**
 * A field to display a version
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class VersionListGridField extends ColoredListGridField {

	private static final String VERSION = "version";

	public VersionListGridField() {
		this(VERSION, VERSION);
	}

	public VersionListGridField(String name) {
		this(name, VERSION);
	}

	public VersionListGridField(String name, String title) {
		super(name, I18N.message(title));
		setAutoFitWidth(true);
		setMinWidth(50);
		setAlign(Alignment.CENTER);
	}
}