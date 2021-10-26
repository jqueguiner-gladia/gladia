package main

import (
	"context"
	"fmt"
	"github.com/labstack/echo/v4"
	"github.com/sirupsen/logrus"
	"net/http/httputil"
	"net/url"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"
	"flag"
)

var onlyOneSignalHandler = make(chan struct{})
var shutdownSignals = []os.Signal{os.Interrupt, syscall.SIGTERM}

// SetupSignalHandler registered for SIGTERM and SIGINT. A stop channel is returned
// which is closed on one of these signals. If a second signal is caught, the program
// is terminated with exit code 1.
func SetupSignalHandler() (stopCh <-chan struct{}) {
	close(onlyOneSignalHandler) // panics when called twice

	stop := make(chan struct{})
	c := make(chan os.Signal, 2)
	signal.Notify(c, shutdownSignals...)
	go func() {
		<-c
		close(stop)
		<-c
		os.Exit(1) // second signal. Exit directly.
	}()

	return stop
}

func main() {
    jobID := flag.String("job_id", "", "")
    token := flag.String("token", "", "")
	port := flag.String("port", "8080", "")
    flag.Parse()

    fmt.Printf("job_id: \"%v\"\n", string(*jobID))
    fmt.Printf("token: \"%v\"\n", string(*token))
	fmt.Printf("port: \"%v\"\n", string(*port))
	exit := SetupSignalHandler()
	ctx, cancelFunc := context.WithCancel(context.Background())
	wg := &sync.WaitGroup{}

	logger := logrus.New()
	echoServer := echo.New()
	echoServer.HideBanner = true

	echoServer.Any("*", TokenMiddleware(string(*jobID), string(*token)))

	go func() {
		wg.Add(1)
		// Start server
		go func() {
			if err := echoServer.Start(":" + string(*port)); err != nil {
				// Occurs after a call to Shutdown or Close
				if err == http.ErrServerClosed {
					logger.Info(err)
				} else {
					logger.Error(err)
				}
			}
		}()

		<-ctx.Done()

		logger.Infof("Exiting API")

		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)

		defer cancel()
		defer wg.Done()

		if err := echoServer.Shutdown(ctx); err != nil {
			logger.Error(err)
		}
	}()

	<- exit
	cancelFunc()

	wg.Wait()
}

func TokenMiddleware(jobID, token string)  echo.HandlerFunc  {

		return func(c echo.Context) error {

			jobHost := fmt.Sprintf("https://%s.job.gra.training.ai.cloud.ovh.net", jobID)
			
			jobURL, err := url.Parse(jobHost)
			fmt.Printf(jobHost)
			
			if err != nil {
				return c.JSON(http.StatusInternalServerError, "parse url error")
			}

			proxy := httputil.NewSingleHostReverseProxy(jobURL)

			proxy.Director = func(request *http.Request) {
				request.Host = jobURL.Host
				request.URL = c.Request().URL
				request.URL.Host = jobURL.Host
				request.URL.Scheme = jobURL.Scheme
			}


			c.Request().Header.Add("Authorization", "Bearer " + token)
			proxy.ServeHTTP(c.Response(), c.Request())
			return nil
		}
}
