package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the window that must be showed to the user during a long LogicalDOC
 * computation.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class ContactingServer extends Window {

	public static ContactingServer instance;

	public static ContactingServer get() {
		if (instance == null)
			instance = new ContactingServer();
		return instance;
	}

	private ContactingServer() {
		setHeaderControls(HeaderControls.HEADER_LABEL);
		setTitle(" ");
		setCanDragResize(false);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);
		setShowStatusBar(false);
		setBodyColor("white");
		setMembersMargin(0);

		HTMLFlow message = new HTMLFlow(AwesomeFactory.getSpinnerIconHtml("pulse", I18N.message("contactingserver")));
		message.setAlign(Alignment.CENTER);
		message.setStyleName("contactingserver");
		message.setLayoutAlign(Alignment.CENTER);
		message.setLayoutAlign(VerticalAlignment.CENTER);
		message.setBackgroundColor("white");
		message.setHeight(50);
		
		
		VLayout container = new VLayout();
		container.setWidth100();
		container.setMembersMargin(5);
		container.setMargin(3);
		container.setAlign(Alignment.CENTER);
		container.setDefaultLayoutAlign(Alignment.CENTER);
		container.setMembers(message);
		
		addItem(container);
	}
}