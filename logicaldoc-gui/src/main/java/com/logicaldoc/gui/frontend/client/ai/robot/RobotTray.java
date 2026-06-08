package com.logicaldoc.gui.frontend.client.ai.robot;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.menu.MenuTray;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * A tray to interact with the Robots
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class RobotTray extends MenuTray {

    private static final String QUESTION = "question";

    public RobotTray() {
        TextItem question = ItemFactory.newTextItem(QUESTION, "");
        question.setShowTitle(false);
        question.setWidth(200);
        question.addKeyPressHandler(event -> {
            if (event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName()))
                onAsk(question.getValueAsString());
        });

        FormItemIcon ask = new FormItemIcon();
        ask.setInline(true);
        ask.setInlineIconAlign(Alignment.RIGHT);
        ask.setText(AwesomeFactory.getIconHtml("paper-plane"));
        ask.setPrompt(I18N.message("ask"));
        ask.addFormItemClickHandler(click -> onAsk(question.getValueAsString()));
        question.setIcons(ask);

        RobotSelector robot = new RobotSelector();
        robot.setDefaultToFirstOption(true);
        robot.setWidth(100);

        setNumCols(3);

        setItems(robot, question);
    }

    private void onAsk(String question) {
        if (validate()) {
            RobotService.Instance.get().get(Long.parseLong(getValueAsString("robot")), new DefaultAsyncCallback<>() {

                @Override
                public void handleSuccess(GUIRobot rbt) {
                    RobotThread.get(rbt).ask(question);
                }
            });
        }
    }

    @Override
    public String getName() {
        return "robot";
    }
}