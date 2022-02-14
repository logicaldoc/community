package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;

/**
 * A field to display a file name with icon
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class VersionListGridField extends ColoredListGridField {

	public VersionListGridField() {
		this("version", "version");
	}

	public VersionListGridField(String name) {
		this(name, "version");
	}

	public VersionListGridField(String name, String title) {
		super(name, I18N.message(title));
		setAutoFitWidth(true);
		setMinWidth(50);
		setAlign(Alignment.CENTER);
	}
}