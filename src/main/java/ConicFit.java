package com.jamesrthompson.Fitting;

import javafx.scene.chart.XYChart;

/**
 * Conic Section Fitting Class
 * @author James R. Thompson, D.Phil
 * @since May 29, 2012 Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci.
 * University of Southern California.
 */
public class ConicFit extends Fit {

    /**
     * Do the fit and store it only as a chart series of those fitting points.
     */
    public void runFit() {
        int np = seriesToFit.getData().size();
        theFit = new XYChart.Series<Number, Number>();
        double D[][] = new double[np + 1][7];
        double S[][] = new double[7][7];
        double Const[][] = new double[7][7];
        double temp[][] = new double[7][7];
        double L[][] = new double[7][7];
        double C[][] = new double[7][7];
        double invL[][] = new double[7][7];
        double d[] = new double[7];
        double V[][] = new double[7][7];
        double sol[][] = new double[7][7];
        double tx, ty;
        int nrot = 0;
        int npts = np;
        double XY[][] = new double[3][npts + 1];
        double pvec[] = new double[7];
        Const[1][1] = 2;
        Const[2][2] = 1;
        Const[3][3] = 2;
        if (np < 6) {
            return; // Should basically break if failed.
        }
        for (int i = 1; i <= np; i++) {
            tx = seriesToFit.getData().get(i - 1).getXValue().doubleValue();
            ty = seriesToFit.getData().get(i - 1).getYValue().doubleValue();
            D[i][1] = tx * tx;
            D[i][2] = tx * ty;
            D[i][3] = ty * ty;
            D[i][4] = tx;
            D[i][5] = ty;
            D[i][6] = 1.0;
        }
        A_TforB(D, D, S, np, 6, np, 6);
        choleskyDecomposition(S, 6, L);
        inverse(L, invL, 6);
        AforB_T(Const, invL, temp, 6, 6, 6, 6);
        AforB(invL, temp, C, 6, 6, 6, 6);
        jacobian(C, 6, d, V, nrot);
        A_TforB(invL, V, sol, 6, 6, 6, 6);
        for (int j = 1; j <= 6; j++) {
            double mod = 0.0;
            for (int i = 1; i <= 6; i++) {
                mod += sol[i][j] * sol[i][j];
            }
            for (int i = 1; i <= 6; i++) {
                sol[i][j] /= Math.sqrt(mod);
            }
        }
        double zero = 10e-20;
        double minev = 10e+20;
        int solind = 0;
        for (int i = 1; i <= 6; i++) {
            if (d[i] < minev && Math.abs(d[i]) > zero) {
                solind = i;
            }
        }
        for (int j = 1; j <= 6; j++) {
            pvec[j] = sol[j][solind];
        }
        draw_conic(pvec, npts, XY);
        for (int i = npts - 1; i >= 0; i--) {
            theFit.getData().add(new XYChart.Data<Number, Number>(XY[1][i], XY[2][i]));
        }
    }

    /**
     * Do the fit and return a chart series of those fitting points.
     *
     * @return - A chart series of those fitting points.
     */
    public XYChart.Series<Number, Number> getFit() {
        int np = seriesToFit.getData().size();
        theFit = new XYChart.Series<Number, Number>();
        double D[][] = new double[np + 1][7];
        double S[][] = new double[7][7];
        double Const[][] = new double[7][7];
        double temp[][] = new double[7][7];
        double L[][] = new double[7][7];
        double C[][] = new double[7][7];
        double invL[][] = new double[7][7];
        double d[] = new double[7];
        double V[][] = new double[7][7];
        double sol[][] = new double[7][7];
        double tx, ty;
        int nrot = 0;
        int npts = np;
        double XY[][] = new double[3][npts + 1];
        double pvec[] = new double[7];
        Const[1][1] = 2;
        Const[2][2] = 1;
        Const[3][3] = 2;
        if (np < 6) {
            return theFit; // Should contain nothing if failed.
        }
        for (int i = 1; i <= np; i++) {
            tx = seriesToFit.getData().get(i - 1).getXValue().doubleValue();
            ty = seriesToFit.getData().get(i - 1).getYValue().doubleValue();
            D[i][1] = tx * tx;
            D[i][2] = tx * ty;
            D[i][3] = ty * ty;
            D[i][4] = tx;
            D[i][5] = ty;
            D[i][6] = 1.0;
        }
        A_TforB(D, D, S, np, 6, np, 6);
        choleskyDecomposition(S, 6, L);
        inverse(L, invL, 6);
        AforB_T(Const, invL, temp, 6, 6, 6, 6);
        AforB(invL, temp, C, 6, 6, 6, 6);
        jacobian(C, 6, d, V, nrot);
        A_TforB(invL, V, sol, 6, 6, 6, 6);
        for (int j = 1; j <= 6; j++) {
            double mod = 0.0;
            for (int i = 1; i <= 6; i++) {
                mod += sol[i][j] * sol[i][j];
            }
            for (int i = 1; i <= 6; i++) {
                sol[i][j] /= Math.sqrt(mod);
            }
        }
        double zero = 10e-20;
        double minev = 10e+20;
        int solind = 0;
        for (int i = 1; i <= 6; i++) {
            if (d[i] < minev && Math.abs(d[i]) > zero) {
                solind = i;
            }
        }
        for (int j = 1; j <= 6; j++) {
            pvec[j] = sol[j][solind];
        }
        draw_conic(pvec, npts, XY);
        for (int i = npts - 1; i >= 0; i--) {
            theFit.getData().add(new XYChart.Data<Number, Number>(XY[1][i], XY[2][i]));
        }
        return theFit;
    }

    private void multMatrix(double m[][], double g[][], double mg[][]) {
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 2; j++) {
                mg[i][j] = 0;
            }
        }
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 2; j++) {
                for (int k = 0; k < 4; k++) {
                    mg[i][j] = mg[i][j] + (m[i][k] * g[k][j]);
                }
            }
        }
    }

    private void rotate(double a[][], int i, int j, int k, int l, double tau, double s) {
        double g, h;
        g = a[i][j];
        h = a[k][l];
        a[i][j] = g - s * (h + g * tau);
        a[k][l] = h + s * (g - h * tau);
    }

    private void jacobian(double a[][], int n, double d[], double v[][], int nrot) {
        int j, iq, ip, i;
        double tresh, theta, tau, t, sm, s, h, g, c;
        double b[] = new double[n + 1];
        double z[] = new double[n + 1];
        for (ip = 1; ip <= n; ip++) {
            for (iq = 1; iq <= n; iq++) {
                v[ip][iq] = 0.0;
            }
            v[ip][ip] = 1.0;
        }
        for (ip = 1; ip <= n; ip++) {
            b[ip] = d[ip] = a[ip][ip];
            z[ip] = 0.0;
        }
        nrot = 0;
        for (i = 1; i <= 50; i++) {
            sm = 0.0;
            for (ip = 1; ip <= n - 1; ip++) {
                for (iq = ip + 1; iq <= n; iq++) {
                    sm += Math.abs(a[ip][iq]);
                }
            }
            if (sm == 0.0) {
                return;
            }
            if (i < 4) {
                tresh = 0.2 * sm / (n * n);
            } else {
                tresh = 0.0;
            }
            for (ip = 1; ip <= n - 1; ip++) {
                for (iq = ip + 1; iq <= n; iq++) {
                    g = 100.0 * Math.abs(a[ip][iq]);
                    if (i > 4 && Math.abs(d[ip]) + g == Math.abs(d[ip])
                            && Math.abs(d[iq]) + g == Math.abs(d[iq])) {
                        a[ip][iq] = 0.0;
                    } else if (Math.abs(a[ip][iq]) > tresh) {
                        h = d[iq] - d[ip];
                        if (Math.abs(h) + g == Math.abs(h)) {
                            t = (a[ip][iq]) / h;
                        } else {
                            theta = 0.5 * h / (a[ip][iq]);
                            t = 1.0 / (Math.abs(theta) + Math.sqrt(1.0 + theta * theta));
                            if (theta < 0.0) {
                                t = -t;
                            }
                        }
                        c = 1.0 / Math.sqrt(1 + t * t);
                        s = t * c;
                        tau = s / (1.0 + c);
                        h = t * a[ip][iq];
                        z[ip] -= h;
                        z[iq] += h;
                        d[ip] -= h;
                        d[iq] += h;
                        a[ip][iq] = 0.0;
                        for (j = 1; j <= ip - 1; j++) {
                            rotate(a, j, ip, j, iq, tau, s);
                        }
                        for (j = ip + 1; j <= iq - 1; j++) {
                            rotate(a, ip, j, j, iq, tau, s);
                        }
                        for (j = iq + 1; j <= n; j++) {
                            rotate(a, ip, j, iq, j, tau, s);
                        }
                        for (j = 1; j <= n; j++) {
                            rotate(v, j, ip, j, iq, tau, s);
                        }
                        ++nrot;
                    }
                }
            }
            for (ip = 1; ip <= n; ip++) {
                b[ip] += z[ip];
                d[ip] = b[ip];
                z[ip] = 0.0;
            }
        }
    }

    // Perform the Cholesky decomposition    
    // Return the lower triangular L such that L*L'=A  
    private void choleskyDecomposition(double a[][], int n, double l[][]) {
        int i, j, k;
        double sum;
        double p[] = new double[n + 1];
        for (i = 1; i <= n; i++) {
            for (j = i; j <= n; j++) {
                for (sum = a[i][j], k = i - 1; k >= 1; k--) {
                    sum -= a[i][k] * a[j][k];
                }
                if (i == j) {
                    if (sum <= 0.0) {
                    } else {
                        p[i] = Math.sqrt(sum);
                    }
                } else {
                    a[j][i] = sum / p[i];
                }
            }
        }
        for (i = 1; i <= n; i++) {
            for (j = i; j <= n; j++) {
                if (i == j) {
                    l[i][i] = p[i];
                } else {
                    l[j][i] = a[j][i];
                    l[i][j] = 0.0;
                }
            }
        }
    }

    private int inverse(double TB[][], double InvB[][], int N) {
        int k, i, j, p, q;
        double mult;
        double D, temp;
        double maxpivot;
        int npivot;
        double B[][] = new double[N + 1][N + 2];
        double A[][] = new double[N + 1][2 * N + 2];
        double C[][] = new double[N + 1][N + 1];
        double eps = 10e-20;
        for (k = 1; k <= N; k++) {
            for (j = 1; j <= N; j++) {
                B[k][j] = TB[k][j];
            }
        }
        for (k = 1; k <= N; k++) {
            for (j = 1; j <= N + 1; j++) {
                A[k][j] = B[k][j];
            }
            for (j = N + 2; j <= 2 * N + 1; j++) {
                A[k][j] = (float) 0;
            }
            A[k][k - 1 + N + 2] = (float) 1;
        }
        for (k = 1; k <= N; k++) {
            maxpivot = Math.abs(A[k][k]);
            npivot = k;
            for (i = k; i <= N; i++) {
                if (maxpivot < Math.abs(A[i][k])) {
                    maxpivot = Math.abs(A[i][k]);
                    npivot = i;
                }
            }
            if (maxpivot >= eps) {
                if (npivot != k) {
                    for (j = k; j <= 2 * N + 1; j++) {
                        temp = A[npivot][j];
                        A[npivot][j] = A[k][j];
                        A[k][j] = temp;
                    }
                }
                D = A[k][k];
                for (j = 2 * N + 1; j >= k; j--) {
                    A[k][j] = A[k][j] / D;
                }
                for (i = 1; i <= N; i++) {
                    if (i != k) {
                        mult = A[i][k];
                        for (j = 2 * N + 1; j >= k; j--) {
                            A[i][j] = A[i][j] - mult * A[k][j];
                        }
                    }
                }
            } else {
                return (-1);
            }
        }
        for (k = 1, p = 1; k <= N; k++, p++) {
            for (j = N + 2, q = 1; j <= 2 * N + 1; j++, q++) {
                InvB[p][q] = A[k][j];
            }
        }
        return (0);
    }

    private void AforB(double _A[][], double _B[][], double _res[][], int _righA, int _colA, int _righB, int _colB) {
        int p, q, l;
        for (p = 1; p <= _righA; p++) {
            for (q = 1; q <= _colB; q++) {
                _res[p][q] = 0.0;
                for (l = 1; l <= _colA; l++) {
                    _res[p][q] = _res[p][q] + _A[p][l] * _B[l][q];
                }
            }
        }
    }

    private void A_TforB(double _A[][], double _B[][], double _res[][],
                         int _righA, int _colA, int _righB, int _colB) {
        int p, q, l;
        for (p = 1; p <= _colA; p++) {
            for (q = 1; q <= _colB; q++) {
                _res[p][q] = 0.0;
                for (l = 1; l <= _righA; l++) {
                    _res[p][q] = _res[p][q] + _A[l][p] * _B[l][q];
                }
            }
        }
    }

    private void AforB_T(double _A[][], double _B[][], double _res[][],
                         int _righA, int _colA, int _righB, int _colB) {
        int p, q, l;
        for (p = 1; p <= _colA; p++) {
            for (q = 1; q <= _colB; q++) {
                _res[p][q] = 0.0;
                for (l = 1; l <= _righA; l++) {
                    _res[p][q] = _res[p][q] + _A[p][l] * _B[q][l];
                }
            }
        }
    }

    private void draw_conic(double pvec[], int nptsk, double points[][]) {
        int npts = nptsk / 2;
        double u[][] = new double[3][npts + 1];
        double Aiu[][] = new double[3][npts + 1];
        double L[][] = new double[3][npts + 1];
        double B[][] = new double[3][npts + 1];
        double Xpos[][] = new double[3][npts + 1];
        double Xneg[][] = new double[3][npts + 1];
        double ss1[][] = new double[3][npts + 1];
        double ss2[][] = new double[3][npts + 1];
        double lambda[] = new double[npts + 1];
        double uAiu[][] = new double[3][npts + 1];
        double A[][] = new double[3][3];
        double Ai[][] = new double[3][3];
        double Aib[][] = new double[3][2];
        double b[][] = new double[3][2];
        double r1[][] = new double[2][2];
        double Ao, Ax, Ay, Axx, Ayy, Axy;
        double theta;
        int i;
        int j;
        double kk;
        Ao = pvec[6];
        Ax = pvec[4];
        Ay = pvec[5];
        Axx = pvec[1];
        Ayy = pvec[3];
        Axy = pvec[2];
        A[1][1] = Axx;
        A[1][2] = Axy / 2;
        A[2][1] = Axy / 2;
        A[2][2] = Ayy;
        b[1][1] = Ax;
        b[2][1] = Ay;
        for (i = 1, theta = 0.0; i <= npts; i++, theta += (Math.PI / npts)) {
            u[1][i] = Math.cos(theta);
            u[2][i] = Math.sin(theta);
        }
        inverse(A, Ai, 2);
        AforB(Ai, b, Aib, 2, 2, 2, 1);
        A_TforB(b, Aib, r1, 2, 1, 2, 1);
        r1[1][1] = r1[1][1] - 4 * Ao;
        AforB(Ai, u, Aiu, 2, 2, 2, npts);
        for (i = 1; i <= 2; i++) {
            for (j = 1; j <= npts; j++) {
                uAiu[i][j] = u[i][j] * Aiu[i][j];
            }
        }
        for (j = 1; j <= npts; j++) {
            if ((kk = (r1[1][1] / (uAiu[1][j] + uAiu[2][j]))) >= 0.0) {
                lambda[j] = Math.sqrt(kk);
            } else {
                lambda[j] = -1.0;
            }
        }
        for (j = 1; j <= npts; j++) {
            L[1][j] = L[2][j] = lambda[j];
        }
        for (j = 1; j <= npts; j++) {
            B[1][j] = b[1][1];
            B[2][j] = b[2][1];
        }
        for (j = 1; j <= npts; j++) {
            ss1[1][j] = 0.5 * (L[1][j] * u[1][j] - B[1][j]);
            ss1[2][j] = 0.5 * (L[2][j] * u[2][j] - B[2][j]);
            ss2[1][j] = 0.5 * (-L[1][j] * u[1][j] - B[1][j]);
            ss2[2][j] = 0.5 * (-L[2][j] * u[2][j] - B[2][j]);
        }
        AforB(Ai, ss1, Xpos, 2, 2, 2, npts);
        AforB(Ai, ss2, Xneg, 2, 2, 2, npts);
        for (j = 1; j <= npts; j++) {
            if (lambda[j] == -1.0) {
                points[1][j] = -1.0;
                points[2][j] = -1.0;
                points[1][j + npts] = -1.0;
                points[2][j + npts] = -1.0;
            } else {
                points[1][j] = Xpos[1][j];
                points[2][j] = Xpos[2][j];
                points[1][j + npts] = Xneg[1][j];
                points[2][j + npts] = Xneg[2][j];
            }
        }
    }
}