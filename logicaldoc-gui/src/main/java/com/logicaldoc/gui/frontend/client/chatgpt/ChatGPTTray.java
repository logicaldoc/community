package com.logicaldoc.gui.frontend.client.chatgpt;

import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.menu.MenuTray;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * A tray to interact with the ChatGPT AI
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class ChatGPTTray extends MenuTray {

	public ChatGPTTray() {
		TextItem question = ItemFactory.newTextItem("chatgpt", "");
		question.setWidth(250);
		question.setRequired(true);
		question.addKeyPressHandler(event -> {
			if (event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName()))
				onAsk(question.getValueAsString());
		});

		FormItemIcon ask = new FormItemIcon();
		ask.setInline(true);
		ask.setInlineIconAlign(Alignment.RIGHT);
		ask.setText(AwesomeFactory.getIconHtml("paper-plane"));
		question.setIcons(ask);
		ask.addFormItemClickHandler(click -> onAsk(question.getValueAsString()));

		setItems(question);
	}

	private void onAsk(String question) {
		if(!validate())
			return;
		
		if (DocumentController.get().getCurrentSelection().isEmpty())
			SC.warn(I18N.message("nodocsselected"));
		else
			ChatGPTThread.get().open(question);
	}
}