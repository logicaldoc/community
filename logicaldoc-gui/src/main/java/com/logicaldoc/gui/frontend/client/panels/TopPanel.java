package com.logicaldoc.gui.frontend.client.panels;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.BackgroundRepeat;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * The banner panel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TopPanel extends HLayout {
	public TopPanel() {
		setWidth100();
		if (Session.get().getConfigAsBoolean("gui.banner")) {
			setBackgroundImage(Session.get().getInfo().getBranding().getBanner());
			setBackgroundRepeat(BackgroundRepeat.REPEAT_X);
			setBackgroundPosition("top left");

			setHeight(45);

			// Prepare the logo image to be shown inside the banner
			Img logoImage = ItemFactory.newBrandImg("logo_head.png", Session.get().getInfo());
			logoImage.setStyleName("logo_head");
			logoImage.setWidth(205);
			logoImage.setHeight(40);
			logoImage.setTooltip(Session.get().getInfo().getBranding().getProductName());
			addMember(logoImage);

			Img separator = ItemFactory.newImg("blank.gif");
			separator.setWidth100();
			addMember(separator);

			// Prepare the OEM logo image to be shown inside the banner
			Img logoOemImage = ItemFactory.newBrandImg("logo_head_oem.png", Session.get().getInfo());
			logoOemImage.setStyleName("logo_head_oem");
			logoOemImage.setWidth(205);
			logoOemImage.setHeight(40);
			logoOemImage.setTooltip(Session.get().getInfo().getBranding().getProductName());
			addMember(logoOemImage);
		} else {
			setHeight(0);
		}
	}
}