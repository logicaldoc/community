package com.logicaldoc.gui.frontend.client.ai.model;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;

public class AIStats extends Window{

	public AIStats() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("aistats"));
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		setAutoCenter(true);
		
		addItem(new AIStatsPanel());
	}
}