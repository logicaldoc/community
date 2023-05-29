package com.logicaldoc.gui.frontend.client.system;

import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Progressbar;
import com.smartgwt.client.widgets.layout.HLayout;

public class ProgressCanvas extends HLayout {

	private Progressbar progressBar = new Progressbar();

	public ProgressCanvas(int progress) {

		setHeight(15);
		setAlign(Alignment.CENTER);

		Progressbar progrBar = new Progressbar();
		progrBar.setHeight(8);
		progrBar.setMargin(3);
		progrBar.setVertical(false);
		progrBar.setShowTitle(true);

		Img image = ItemFactory.newImgIcon("paste.gif");
		
		image.setWidth("16px");
		image.setHeight("16px");

		addMember(progrBar);
		addMember(image);

		setProgress(progress);
	}

	public void setProgress(int progress) {
		progressBar.setPercentDone(progress);
		progressBar.setTitle(progress + "%");
		progressBar.redraw();
	}
}
