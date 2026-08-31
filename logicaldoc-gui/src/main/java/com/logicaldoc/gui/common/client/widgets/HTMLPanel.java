package com.logicaldoc.gui.common.client.widgets;

import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.HTMLFlow;

public class HTMLPanel extends HTMLFlow {

	public HTMLPanel(String html) {
		super();
		setOverflow(Overflow.AUTO);
		setPadding(10);
		setContents(html);
	}

}
