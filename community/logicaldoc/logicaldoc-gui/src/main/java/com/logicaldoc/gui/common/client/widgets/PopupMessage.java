package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.logicaldoc.gui.common.client.Session;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the window that can be used to show a popup a message
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public class PopupMessage extends Window {

	public PopupMessage(String title, String messageText) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(title);
		setAlign(Alignment.CENTER);
		setAutoSize(true);
		centerInPage();
		
		Label label=new Label(messageText);
		label.setMinWidth(350);
		
		VLayout layout= new VLayout();
		layout.setMembersMargin(2);
		layout.setMembers(label);
		
		addItem(layout);
		
		Scheduler.get().scheduleFixedDelay(new RepeatingCommand() {

			@Override
			public boolean execute() {
				PopupMessage.this.destroy();
				return false;
			}
		}, Session.get().getConfigAsInt("gui.popup.timeout") * 1000);
	}
}