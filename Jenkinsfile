pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
               echo 'Building...'
               sh 'npm install'
               sh 'shadow-cljs release :app'
            }
        }
        stage('Test') {
            steps {
            echo "Testing..."
                sh 'shadow-cljs compile test && node out/node-tests.js'
            }
        }

        stage("Deploy") {
            steps {
                echo 'Deploy!'
                sh 'cp -r /var/lib/jenkins/workspace/editoreable/resources /var/www/editoreable.erkanp.dev/html/'
            }
        }
    }
}
