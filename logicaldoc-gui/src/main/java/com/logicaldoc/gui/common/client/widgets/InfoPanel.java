package com.logicaldoc.gui.common.client.widgets;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * An useful panel to show informations to the user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class InfoPanel extends HLayout {

	private Label label = null;

	private boolean alignLeft = false;

	public InfoPanel(String message) {
		setHeight(20);
		setStyleName("infoPanel");
		setMessage(message);
	}

	public void setMessage(String message) {
		if (label != null)
			removeMember(label);
		label = new Label(message);
		label.setWrap(false);
		label.setMargin(3);
		label.setStyleName("infoPanel");
		if (!alignLeft)
			setAlign(Alignment.RIGHT);
		else
			setAlign(Alignment.LEFT);
		setMembers(label);
	}

	public void setAlignLeft(boolean alignLeft) {
		this.alignLeft = alignLeft;
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}