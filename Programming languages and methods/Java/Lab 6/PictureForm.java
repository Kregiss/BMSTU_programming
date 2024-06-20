import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.geom.AffineTransform;

public class PictureForm {
    private JPanel mainPanel ;
    private CanvasPanel canvasPanel;
    private JSpinner spinnerA;
    private JSpinner spinnerB;
    private JSpinner spinnerUgol;


    public PictureForm ( ) {

        spinnerA.addChangeListener (new ChangeListener ( ) {
            public void stateChanged ( ChangeEvent e ) {
                int a = (int)(Integer)spinnerA.getValue();
                canvasPanel.setA(a);
            }
        });

        spinnerB.addChangeListener (new ChangeListener ( ) {
            public void stateChanged ( ChangeEvent e ) {
                int b = (int)(Integer)spinnerB.getValue();
                canvasPanel.setB(b);
            }
        });

        spinnerUgol.addChangeListener (new ChangeListener ( ) {
            public void stateChanged ( ChangeEvent e ) {
                int ugol = (int)(Integer)spinnerUgol.getValue();
                canvasPanel.setUgol(ugol);
            }
        });

        spinnerA.setValue(100);
        spinnerB.setValue(50);
        spinnerUgol.setValue(30);
    }

    public static void main (String[ ] args) {
        JFrame frame = new JFrame("Прямоугольник");
        frame.setContentPane (new PictureForm().mainPanel) ;
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE) ;
        frame.pack();
        frame.setVisible(true) ;
    }

    public static class CanvasPanel extends JPanel {
        int a, b;

        double ugol;

        public void setA(int A) {
            a = A;
            repaint();
        }

        public void setB(int B) {
            b = B;
            repaint();
        }
        public void setUgol(int ugol1) {
            ugol = ugol1;
            repaint();
        }

        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            Graphics2D g2d = (Graphics2D) g;

            // Находим центр панели
            int centerX = getWidth() / 2;
            int centerY = getHeight() / 2;

            // Рисуем оси координат
            g2d.setColor(Color.BLACK);
            g2d.drawLine(0, getHeight() / 2, getWidth(), getHeight() / 2); // Ось X
            g2d.drawLine(getWidth() / 2, 0, getWidth() / 2, getHeight()); // Ось Y

            Rectangle rectangle = new Rectangle(centerX - a / 2, centerY - b / 2, a, b);

            AffineTransform transform = new AffineTransform();
            if (a > b){
                transform.rotate(Math.toRadians(180 - (ugol % 360)), centerX, centerY);
                g2d.setTransform(transform);
            } else {
                transform.rotate(Math.toRadians(90 - ugol), centerX, centerY);
                g2d.setTransform(transform);
            }

            g2d.setColor(Color.RED);
            g2d.draw(rectangle);
        }
    }

    private void createUIComponents() {
        canvasPanel = new CanvasPanel();
    }
}