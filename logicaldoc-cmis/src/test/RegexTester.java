package com.logicaldoc.cmis;

import java.applet.Applet;
import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Label;
import java.awt.Panel;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * This is a program for trying out regular expressions.
 * Java 1.4 or better is required.
 *
 * @author David Matuszek 
 * @version 2.0
 */
public class RegexTester extends Applet implements ActionListener, TextListener {
	
    private static final long serialVersionUID = 1L;
    
    // GUI components
    Button matchesButton = new Button("matches()");
    Button lookingAtButton = new Button("lookingAt()");
    Button findButton = new Button("find()");
    Button resetButton = new Button("reset()");
    
    TextField targetField = new TextField();
    TextField patternField = new TextField();
    TextField javaField = new TextField();
    TextArea resultField = new TextArea();
    
    // Working variables
    String patternText = "";
    String targetText = "";
    Pattern pattern = Pattern.compile(patternText);
    Matcher matcher = pattern.matcher(targetText);
    boolean errorInPattern = false;

    /**
     * Lays out the GUI and adds the action listeners.
     */
    public void init() {
        Font originalFont = getFont();
        Font bigFont = new Font("Monospaced", Font.PLAIN, 16);

        // Lay out components and set fonts
        Panel appletPanel = this;
            appletPanel.setLayout(new BorderLayout());
            Panel inputsPanel = new Panel();
                appletPanel.add(BorderLayout.NORTH, inputsPanel);
                inputsPanel.setLayout(new BorderLayout());
                Panel labelPanel = new Panel();
                    inputsPanel.add(BorderLayout.WEST, labelPanel);
                    labelPanel.setLayout(new GridLayout(4, 1));
                    labelPanel.add(new Label(" String:"));
                    labelPanel.add(new Label(" Pattern: "));
                    labelPanel.add(new Label(""));
                    labelPanel.add(new Label(" In Java: "));
                Panel workPanel = new Panel();
                    inputsPanel.add(BorderLayout.CENTER, workPanel);
                    workPanel.setLayout(new GridLayout(4, 1));
                    workPanel.setFont(bigFont);
                    workPanel.add(targetField);
                    workPanel.add(patternField);
                    Panel buttonPanel = new Panel();
                        workPanel.add(buttonPanel);
                        buttonPanel.setFont(originalFont);
                        buttonPanel.setLayout(new GridLayout(1, 4));
                        buttonPanel.add(matchesButton);
                        buttonPanel.add(lookingAtButton);
                        buttonPanel.add(findButton);
                        buttonPanel.add(resetButton);
                    Panel javaPanel = new Panel();
                        workPanel.add(javaPanel);
                        javaPanel.setLayout(new GridLayout(1, 1));
                            javaPanel.add(javaField);
            Panel resultsPanel = new Panel();
                appletPanel.add(BorderLayout.CENTER, resultsPanel);
                resultsPanel.setLayout(new BorderLayout());
                Panel label2Panel = new Panel();
                    resultsPanel.add(BorderLayout.WEST, label2Panel);
                    label2Panel.add(BorderLayout.CENTER, new Label("Result:"));
                    add(BorderLayout.CENTER, resultsPanel);
                resultsPanel.add(BorderLayout.CENTER, resultField);
                resultField.setFont(bigFont);
        
        // Add listeners
        matchesButton.addActionListener(this);
        lookingAtButton.addActionListener(this);
        findButton.addActionListener(this);
        resetButton.addActionListener(this);
        targetField.addTextListener(this);
        patternField.addTextListener(this);
        
        // Set the initial state of the GUI
        setSize(700, 400); // appletviewer only
        resetButton.setEnabled(false);
        javaField.setEditable(false);
    }

    /**
     * Handles button clicks.
     */
    public void actionPerformed(ActionEvent event) {
        ensureMatcherIsCurrent();
        Button source = (Button)event.getSource();
        boolean success = false;
        try {
            if (errorInPattern) {
                resultField.setText("PatternSyntaxException");
                return;
            }
            if (source == matchesButton) {
                success = matcher.matches();
                resetButton.setEnabled(false);
            } else if (source == lookingAtButton) {
                success = matcher.lookingAt();
                resetButton.setEnabled(success);
            } else if (source == findButton) {
                success = matcher.find();
                resetButton.setEnabled(success);
            } else if (source == resetButton) {
                resetMatcher();
                return;
            } 
            
            if (success) {
                showResult();
            } else {
                resultField.setText("Pattern did not match.");
                /* Here we handle a possible bug in the Matcher.find()
                 * method. If the last thing matched by find() is an
                 * empty string, and then the find() fails, the matcher
                 * is <i>not</i> reset. In this case, we enable the reset()
                 * button. Unfortunately, this also means that the reset()
                 * button will never be disabled when the pattern just
                 * doesn't match anything in the string.
                 */
                if (source != findButton) return;
                if (matcher.find()) {
                    matcher.reset();
                } else {
                    resetButton.setEnabled(true);
                }
            }
        }
        catch (IllegalStateException e) {
            resultField.setText("IllegalStateException: " + e.getMessage());
        }
    }
    
    /**
     * Updates the Java string field and erases the results field
     * whenever the target string or the pattern string is changed.
     */
    public void textValueChanged(TextEvent e) {
        TextField inputField = (TextField)e.getSource();
        if (inputField == patternField) {
            javaField.setText("\"" + javaVersion(patternField.getText()) + "\"");
        }
        resetMatcher();
    }
    
    /**
     * Displays the result of the most recent match.
     */
    void showResult() {
        String result = "";
        if (targetText == null) result = "String to be searched is null.";
        else {
            result = "start() = " + matcher.start() + 
                     ", end() = " + matcher.end() + "\n";
            for (int i = 0; i <= matcher.groupCount(); i++) {
                if (i > 0) result += "\n";
                result += "group(" + i + ") = \"" + matcher.group(i) + "\"";
            }
        }
        resultField.setText(result);
    }
    
    /**
     * If the pattern or the target string has changed, resets
     * everything; otherwise, does nothing.
     */
    void ensureMatcherIsCurrent() {
        if (patternText.equals(patternField.getText()) &&
            targetText.equals(targetField.getText()))
            return;
        resetMatcher();
    }
    
    /**
     * Converts a String to Java literal string syntax by inserting
     * a backslash before characters that must be quoted. Double
     * quotes are <i>not</i> placed around the output string.
     * 
     * @param regex The string to be converted.
     * @return The way this string should be written in Java.
     */
    String javaVersion(String regex) {
        String charsToQuote = "\b\t\n\r\"\'\\";
        StringBuilder buffer = new StringBuilder(regex);
        for (int i = regex.length() - 1; i >= 0; i--) {
            if (charsToQuote.indexOf(buffer.charAt(i)) >= 0) {
                buffer.insert(i, '\\');
            }
        }
        return new String(buffer);
    }
    
    /**
     * Resets the pattern matcher and disables the Reset button.
     */
    void resetMatcher() {
        patternText = patternField.getText();
        targetText = targetField.getText();
        try {
            errorInPattern = false;
            pattern = Pattern.compile(patternText);
        }
        catch(PatternSyntaxException e) {
            errorInPattern = true;
        }
        matcher = pattern.matcher(targetText);
        resultField.setText("");
        resetButton.setEnabled(false);
    }
}
