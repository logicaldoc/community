package com.logicaldoc.i18n;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Properties;
import java.util.Vector;

/**
 * This is an extension of Java Properties that stores the properties
 * alphabetically
 *
 * @author Brian Pipa - http://pipasoft.com
 * @version 1.0
 */
public class OrderedProperties extends Properties {

    private static final long serialVersionUID = 1L;

    /**
     * Overrides the original store() method and sorts the output
     *
     * @param out a FileOutPutStream to send the output to
     * @param header a textual header for the top of the file
     * @exception IOException when things go wrong
     */
    @Override
    public void store(OutputStream out, String header) throws IOException {
        Vector<String> keys = new Vector<>();

        for (Enumeration e = this.propertyNames(); e.hasMoreElements();)
            keys.addElement((String) (e.nextElement()));

        // sort them
        Collections.sort(keys);

        // write the header
        DataOutputStream dataoutputstream = new DataOutputStream(out);
        dataoutputstream.writeBytes("#%s%n".formatted(header));

        // write the date/time
        dataoutputstream.writeBytes("#%s%n".formatted(LocalDateTime.now(ZoneId.systemDefault()).toString()));

        // now, loop through and write out the properties
        String oneline;
        String thekey;
        String thevalue;

        for (int i = 0; i < keys.size(); i++) {
            thekey = (String) keys.elementAt(i);
            thevalue = (String) this.getProperty(thekey);
            thevalue = doubleSlash(thevalue);

            oneline = "%s=%s%n".formatted(thekey, thevalue);
            dataoutputstream.writeBytes(oneline);
        }

        dataoutputstream.flush();
        dataoutputstream.close();
    }

    /**
     * Private method to double slash paths
     *
     * @param orig the string to double slash
     * @return a double-slashed string
     */
    private String doubleSlash(String orig) {
        StringBuilder buf = new StringBuilder();

        for (int i = 0; i < orig.length(); i++) {
            if (orig.charAt(i) == '\\') {
                buf.append("\\\\");
            } else {
                buf.append(orig.charAt(i));
            }
        }

        return buf.toString();
    }
}
