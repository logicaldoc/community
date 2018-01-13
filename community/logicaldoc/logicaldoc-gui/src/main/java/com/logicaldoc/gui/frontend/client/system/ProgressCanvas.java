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

		Progressbar progressBar = new Progressbar();
		progressBar.setHeight(8);
		progressBar.setMargin(3);
		progressBar.setVertical(false);
		progressBar.setShowTitle(true);

		boolean enabled = true;
		// record.getAttributeAsBoolean("eenabled");
		boolean running = true;
		// record.getAttributeAsInt("status") ==
		// GUITask.STATUS_RUNNING;
		Img image = new Img();
		if (enabled && running) {
			image = ItemFactory.newImgIcon("paste.gif");
		} else {
			image = ItemFactory.newImgIcon("document_lock.png");
		}

		image.setWidth("16px");
		image.setHeight("16px");

		addMember(progressBar);
		addMember(image);

		setProgress(progress);
	}

	public void setProgress(int progress) {
		progressBar.setPercentDone(progress);
		progressBar.setTitle(progress + "%");
		progressBar.redraw();
	}
}
