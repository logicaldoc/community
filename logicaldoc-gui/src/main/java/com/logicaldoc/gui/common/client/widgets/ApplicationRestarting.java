package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the window that must be showed to the user during the restart
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.1
 */
public class ApplicationRestarting extends Window {

	private static ApplicationRestarting instance;

	private HTMLFlow messageFlow;

	public static ApplicationRestarting get() {
		return get(I18N.message("applicationisrestarting"));
	}

	public static ApplicationRestarting get(String message) {
		if (instance == null)
			instance = new ApplicationRestarting();
		instance.messageFlow.setContents(message);
		return instance;
	}

	private ApplicationRestarting() {
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

		messageFlow = new HTMLFlow();
		messageFlow.setAlign(Alignment.CENTER);
		messageFlow.setStyleName("contactingserver");
		messageFlow.setLayoutAlign(Alignment.CENTER);
		messageFlow.setLayoutAlign(VerticalAlignment.CENTER);
		messageFlow.setBackgroundColor("white");
		messageFlow.setHeight(50);

		VLayout container = new VLayout();
		container.setWidth100();
		container.setMembersMargin(5);
		container.setMargin(3);
		container.setAlign(Alignment.CENTER);
		container.setDefaultLayoutAlign(Alignment.CENTER);
		container.setMembers(messageFlow);

		addItem(container);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((messageFlow == null) ? 0 : messageFlow.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		ApplicationRestarting other = (ApplicationRestarting) obj;
		if (messageFlow == null) {
			if (other.messageFlow != null)
				return false;
		} else if (!messageFlow.equals(other.messageFlow))
			return false;
		return true;
	}
}