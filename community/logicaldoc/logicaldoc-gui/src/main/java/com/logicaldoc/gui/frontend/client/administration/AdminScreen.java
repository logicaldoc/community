package com.logicaldoc.gui.frontend.client.administration;

import com.logicaldoc.gui.frontend.client.system.GeneralPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Represents the Administration Screen
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class AdminScreen extends HLayout {

	private static AdminScreen instance;

	private VLayout rightPanel = new VLayout();

	private Canvas content;

	private AdminScreen() {
		setWidth100();
		setOverflow(Overflow.HIDDEN);

		// Prepare the collapsible menu
		AdminMenu leftMenu = AdminMenu.get();
		leftMenu.setWidth(290);
		leftMenu.setShowResizeBar(true);

		addMember(leftMenu);
		addMember(rightPanel);

		setContent(new GeneralPanel());
	}

	public void setContent(Canvas content) {
		if (this.content != null) {
			if (rightPanel.contains(this.content))
				rightPanel.removeChild(this.content);
			this.content.destroy();
		}
		this.content = content;
		rightPanel.addMember(this.content);
	}

	public static AdminScreen get() {
		if (instance == null)
			instance = new AdminScreen();
		return instance;
	}

	public Canvas getContent() {
		return content;
	}

	public VLayout getRightPanel() {
		return rightPanel;
	}
}