/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
   /* TwoPlotExample.java: a PtPlot of two plots
 Copyright (c) 1998-2002 The Regents of the University of California.
 All rights reserved.
 IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
 SUCH DAMAGE.
 THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
 PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
 CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, 
 ENHANCEMENTS, OR MODIFICATIONS.                PT_COPYRIGHT_VERSION_2
                                                COPYRIGHTENDKEY
@ProposedRating red (eal@eecs.berkeley.edu)
@AcceptedRating red (cxh@eecs.berkeley.edu)
*/
import ptolemy.plot.Plot;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import javax.swing.JFrame;
// The java.io imports are only necessary for the right hand plot.
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
 
public class TwoPlotExample extends JFrame {

/** We use a constructor here so that we can call methods
     *  directly on the Frame.  The main method is static
     *  so getting at the Frame is a little trickier. */
    
  TwoPlotExample() {                                     // Instantiate the two plots
    Plot leftPlot = new Plot();
    Plot rightPlot = new Plot();
    setSize(800, 300);                                 // Set size of toplevel window
     // Create the left plot by calling methods.
    leftPlot.setSize(350, 300);
    leftPlot.setButtons(true);
    leftPlot.setTitle("Left Plot");
    leftPlot.setYRange(-4, 4);
    leftPlot.setXRange(0, 100);
    leftPlot.setXLabel("time");
    leftPlot.setYLabel("value");
    leftPlot.addYTick("-PI", -Math.PI);
    leftPlot.addYTick("-PI/2", -Math.PI/2);
    leftPlot.addYTick("0", 0);
    leftPlot.addYTick("PI/2", Math.PI/2);
    leftPlot.addYTick("PI", Math.PI);
    leftPlot.setMarksStyle("none");
    leftPlot.setImpulses(true);
    boolean first = true;
    for ( int i = 0; i <= 100; i++ ) {
      leftPlot.addPoint(0, (double)i,  5   * Math.cos(Math.PI * i/20), !first);
      leftPlot.addPoint(1, (double)i,  4.5 * Math.cos(Math.PI * i/25), !first);
      leftPlot.addPoint(2, (double)i,  4   * Math.cos(Math.PI * i/30), !first);
      leftPlot.addPoint(3, (double)i,  3.5 * Math.cos(Math.PI * i/35), !first);
      leftPlot.addPoint(4, (double)i,  3   * Math.cos(Math.PI * i/40), !first);
      leftPlot.addPoint(5, (double)i,  2.5 * Math.cos(Math.PI * i/45), !first);
      leftPlot.addPoint(6, (double)i,  2   * Math.cos(Math.PI * i/50), !first);
      leftPlot.addPoint(7, (double)i,  1.5 * Math.cos(Math.PI * i/55), !first);
      leftPlot.addPoint(8, (double)i,  1   * Math.cos(Math.PI * i/60), !first);
      leftPlot.addPoint(9, (double)i,  0.5 * Math.cos(Math.PI * i/65), !first);
      first = false;
    }
    rightPlot.setButtons(true);               // Create right plot by reading in file
    leftPlot.setSize(350, 300);
    File file = new File(".", "data.plt");
    try { rightPlot.clear(true);   rightPlot.read(new FileInputStream(file)); } 
    catch (FileNotFoundException ex) 
              {System.err.println("File not found: " + file + " : " + ex); } 
    catch (IOException ex) 
      { System.err.println("Error reading input: " + file + " : " + ex); }
    rightPlot.setTitle("Right Plot");                       // Override title in file
    GridBagLayout gridbag = new GridBagLayout();                    // Layout 2 plots
    GridBagConstraints c = new GridBagConstraints();
    getContentPane().setLayout(gridbag);
    c.gridx = 0;  c.gridy = 0; c.gridwidth = 1;                    // Handle leftPlot
    gridbag.setConstraints(leftPlot, c);
    getContentPane().add(leftPlot);
    c.gridx = 1;  c.gridy = 0;  c.gridwidth = 1;                  // Handle rightPlot
    c.fill = GridBagConstraints.BOTH;
    c.weightx = 1.0;   c.weighty = 1.0;
    gridbag.setConstraints(rightPlot, c);   getContentPane().add(rightPlot);
    show();
  }

 // main method instantiate class, work in constructor
  public static void main(String args[])
                        {TwoPlotExample twoPlotExample = new TwoPlotExample(); }
}
