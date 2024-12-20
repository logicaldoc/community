package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.widgetideas.graphics.client.ImageLoader;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ImageStyle;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Window;

public class ImageLightbox extends Window {
	public ImageLightbox(String imageUrl, String title, int size) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message(title));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		ImageLoader.loadImages(new String[] { imageUrl },
				imageElements -> {
					Img img = new Img(imageUrl);  
			        img.setImageWidth(size);  
			        img.setImageHeight(size);  
			        img.setImageType(ImageStyle.NORMAL);   
			        addItem(img);  	
				});
		
		

	}
}