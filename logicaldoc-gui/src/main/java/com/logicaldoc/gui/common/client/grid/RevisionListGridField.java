package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;

/**
 * A field to display a revision
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
public class RevisionListGridField extends ColoredListGridField {

	private static final String REVISION = "revision";

	public RevisionListGridField() {
		this(REVISION, REVISION);
	}

	public RevisionListGridField(String name) {
		this(name, REVISION);
	}

	public RevisionListGridField(String name, String title) {
		super(name, I18N.message(title));
		setAutoFitWidth(true);
		setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		setMinWidth(70);
		setAlign(Alignment.CENTER);
	}
}