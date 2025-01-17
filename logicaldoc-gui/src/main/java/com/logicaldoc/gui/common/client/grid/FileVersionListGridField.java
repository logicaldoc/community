package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;

/**
 * A field to display a file name with icon
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class FileVersionListGridField extends ColoredListGridField {

	public FileVersionListGridField() {
		this("fileVersion", "fileversion");
	}

	public FileVersionListGridField(String name) {
		this(name, "fileversion");
	}

	public FileVersionListGridField(String name, String title) {
		super(name, I18N.message(title));
		setAutoFitWidth(true);
		setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		setMinWidth(55);
		setAlign(Alignment.CENTER);
	}
}