package com.logicaldoc.gui.common.client.grid;

import com.smartgwt.client.types.AutoFitWidthApproach;

/**
 * A field to display the ID, it must be bound to a boolean column named id
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class IdListGridField extends ColoredListGridField {

	public IdListGridField() {
		this(null);
	}
	
	public IdListGridField(String label) {
		super("id", label);
		setAutoFitWidth(true);
		setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		setHidden(true);
		setCanGroupBy(false);
		setCanSort(true);
		setCanFilter(true);
		setCanEdit(false);
	}
}